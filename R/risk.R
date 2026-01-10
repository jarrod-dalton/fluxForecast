# ------------------------------------------------------------------------------
# risk() and survival()
# ------------------------------------------------------------------------------

#' Compute event risk curves from a ps_forecast
#'
#' @param x A ps_forecast.
#' @param event Character vector of event types of interest.
#' @param times Optional numeric vector subset of x$times (default all).
#' @param start_time Optional scalar time in x$times at which to define the eligible cohort.
#'   Default is x$time0.
#' @param terminal_events Optional character vector of event types that must be absent by start_time.
#' @param condition_on_events Optional character vector of event types that must be absent by start_time.
#' @param eligible Optional function(snapshot, time, ctx) -> TRUE/FALSE. Evaluated at start_time only.
#' @param ctx Optional list passed to eligible().
#'
#' @return A ps_risk object.
#' @export
risk <- function(
  x,
  event,
  by = c("run", "patient", "patient_draw"),
  times = NULL,
  start_time = NULL,
  terminal_events = NULL,
  condition_on_events = NULL,
  eligible = NULL,
  ctx = NULL
) {
  if (!inherits(x, "ps_forecast")) stop("x must be a ps_forecast.", call. = FALSE)

  event <- unique(as.character(event))
  if (length(event) < 1L) stop("event must be a non-empty character vector.", call. = FALSE)

  if (is.null(times)) {
    times <- x$times
  } else {
    times <- sort(unique(.psf_as_numeric_time(times, name = "times", ctx = ctx)))
    if (!all(times %in% x$times)) stop("All times must be members of x$times.", call. = FALSE)
  }

  if (is.null(start_time)) {
    start_time <- x$time0
  }
  start_time <- .psf_as_numeric_time(start_time, name = "start_time", ctx = ctx)
  if (!is.finite(start_time) || length(start_time) != 1L) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% x$times) stop("start_time must be one of x$times (v1 restriction).", call. = FALSE)

  by <- match.arg(by)

# indices
  t_start_idx <- match(start_time, x$times)
  t_idx <- match(times, x$times)

  R <- nrow(x$run_index)

  # base eligibility: defined & alive at start_time
  # NOTE: forecast() stores non-numeric state as character (e.g., "TRUE"/"FALSE").
  # We must not treat character "TRUE" as NA.
  alive0 <- x$alive[, t_start_idx]
  if (!is.logical(alive0)) {
    alive0 <- suppressWarnings(as.logical(alive0))
  }
  # vectorized TRUE checks (isTRUE() is scalar-only)
  elig <- (x$defined[, t_start_idx] %in% TRUE) & (alive0 %in% TRUE)

  # fast event-based conditioning at start_time
  # helper to compute first time of any event in a set
  first_time_any <- function(ev_set) {
    ev_set <- unique(as.character(ev_set))
    cols <- match(ev_set, x$event_types)
    if (any(is.na(cols))) {
      bad <- ev_set[is.na(cols)]
      stop("Unknown event types: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    # Guard against NA times (e.g., if an event time was missing in inputs).
    # With na.rm=TRUE, rows with no observed events remain Inf.
    ft <- suppressWarnings(apply(x$first_event_time[, cols, drop = FALSE], 1, min, na.rm = TRUE))
    # Defensive: comparisons like (NA <= t) propagate NA, which can turn sums into NA.
    # Treat missing event times as "never".
    ft[is.na(ft)] <- Inf
    # If a row was all-NA, min(..., na.rm=TRUE) returns Inf (with warning); keep that.
    ft
  }

  if (!is.null(terminal_events)) {
    ft <- first_time_any(terminal_events)
    elig <- elig & (ft > start_time)
  }
  if (!is.null(condition_on_events)) {
    ft <- first_time_any(condition_on_events)
    elig <- elig & (ft > start_time)
  }

  # slow-ish predicate evaluated at start_time only
  if (!is.null(eligible)) {
    if (!is.function(eligible)) stop("eligible must be a function if provided.", call. = FALSE)
    # Build snapshot list for each run at start_time using stored state matrices
    # Note: stored values for non-numeric vars are character.
    # We pass a named list 'snapshot'.
    keep <- which(elig)
    if (length(keep) > 0L) {
      ok <- logical(length(keep))
      for (k in seq_along(keep)) {
        i <- keep[[k]]
        snap <- lapply(x$vars, function(v) x$state[[v]][i, t_start_idx])
        names(snap) <- x$vars
        out <- eligible(snap, start_time, ctx)
        ok[[k]] <- identical(out, TRUE)
      }
      elig[keep] <- ok
    }
  }

        
spec <- list(
  event = event,
  times = times,
  start_time = start_time,
  terminal_events = terminal_events,
  condition_on_events = condition_on_events,
  by = by
)

eligible_run_ids <- which(elig)
      n_eligible <- length(eligible_run_ids)

      # grouping:
      #   by = "run"         -> one curve pooling all eligible runs
      #   by = "patient"     -> one curve per patient_id (averaging across that patient's eligible runs)
      #   by = "patient_draw"-> one curve per (patient_id, draw_id)
      group_cols <- switch(
        by,
        run = character(0),
        patient = "patient_id",
        patient_draw = c("patient_id", "draw_id")
      )
      if (length(group_cols) > 0 && !all(group_cols %in% names(x$run_index))) {
        stop("ps_forecast$run_index must include required columns for by=.", call. = FALSE)
      }

      if (n_eligible == 0L) {
  if (length(group_cols) == 0) {
    res <- data.frame(time = times, n_eligible = 0L, n_events = 0L, risk = NA_real_)
  } else {
    # return 0-row data.frame with required columns
    res <- data.frame(time = numeric(0), n_eligible = integer(0), n_events = integer(0), risk = numeric(0))
    for (gc in rev(group_cols)) {
      res[[gc]] <- character(0)
    }
    # reorder so group cols come first
    res <- res[, c(group_cols, "time", "n_eligible", "n_events", "risk"), drop = FALSE]
  }
} else if (length(group_cols) == 0) {
ft_event <- first_time_any(event)
    n_events <- vapply(
      times,
      function(t) {
        leq <- ft_event[eligible_run_ids] <= t
        # comparisons with NA yield NA; treat those as FALSE (no event by t)
        leq[is.na(leq)] <- FALSE
        sum(leq)
      },
      integer(1)
    )
        res <- data.frame(time = times, n_eligible = n_eligible, n_events = n_events, risk = n_events / n_eligible)
      } else {
        idx_elig <- x$run_index[eligible_run_ids, group_cols, drop = FALSE]
        # create a stable group key
        grp <- interaction(idx_elig, drop = TRUE, lex.order = TRUE)
        split_ids <- split(seq_along(eligible_run_ids), grp)

        ft_event_all <- first_time_any(event)
        ft_event_elig <- ft_event_all[eligible_run_ids]

        rows <- vector("list", length(split_ids))
        gi <- 0L
        for (g in names(split_ids)) {
          gi <- gi + 1L
          pos <- split_ids[[g]]
          run_ids_g <- eligible_run_ids[pos]
          n_elig_g <- length(run_ids_g)
          ft_g <- ft_event_elig[pos]
          # count events by time (cumulative)
          n_ev_g <- vapply(times, function(t) sum(!is.na(ft_g) & ft_g <= t), integer(1))
          df_g <- cbind(idx_elig[pos[1], , drop = FALSE],
                        data.frame(time = times, n_eligible = n_elig_g, n_events = n_ev_g, risk = n_ev_g / n_elig_g))
          rownames(df_g) <- NULL
          rows[[gi]] <- df_g
        }
        res <- do.call(rbind, rows)
        rownames(res) <- NULL
      }

  cohort <- list(
    eligible_run_ids = eligible_run_ids,
    n_eligible = n_eligible
  )

  new_ps_risk(spec = spec, cohort = cohort, result = res)
}

#' Event-free curve as pure sugar over risk()
#'
#' @param x A ps_forecast.
#' @param terminal_events Character vector of event types defining the terminal/composite endpoint.
#' @param ... Passed to risk().
#'
#' @return A ps_risk object with result$risk replaced by event_free.
#' @export
survival <- function(x, terminal_events, ...) {
  if (missing(terminal_events) || is.null(terminal_events) || length(terminal_events) < 1L) {
    stop("survival() requires terminal_events (character vector).", call. = FALSE)
  }
  r <- risk(x, event = terminal_events, ...)
  r$result$event_free <- 1 - r$result$risk
  r
}

isTRUEorNA <- function(x) {
  # returns TRUE for TRUE, FALSE for FALSE, and NA stays NA
  ps_na_safe_true(x)
}
