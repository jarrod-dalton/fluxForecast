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
    times <- sort(unique(as.numeric(times)))
    if (!all(times %in% x$times)) stop("All times must be members of x$times.", call. = FALSE)
  }

  if (is.null(start_time)) {
    start_time <- x$time0
  }
  start_time <- as.numeric(start_time)
  if (!is.finite(start_time) || length(start_time) != 1L) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% x$times) stop("start_time must be one of x$times (v1 restriction).", call. = FALSE)

  # indices
  t_start_idx <- match(start_time, x$times)
  t_idx <- match(times, x$times)

  R <- nrow(x$run_index)

  # base eligibility: defined & alive at start_time
  elig <- x$defined[, t_start_idx] & isTRUEorNA(x$alive[, t_start_idx])
  elig[is.na(elig)] <- FALSE

  # fast event-based conditioning at start_time
  # helper to compute first time of any event in a set
  first_time_any <- function(ev_set) {
    ev_set <- unique(as.character(ev_set))
    cols <- match(ev_set, x$event_types)
    if (any(is.na(cols))) {
      bad <- ev_set[is.na(cols)]
      stop("Unknown event types: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    apply(x$first_event_time[, cols, drop = FALSE], 1, min)
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
        ok[[k]] <- isTRUE(eligible(snap, start_time, ctx))
      }
      elig[keep] <- ok
    }
  }

  eligible_run_ids <- which(elig)
  n_eligible <- length(eligible_run_ids)
  if (n_eligible == 0L) {
    res <- data.frame(time = times, n_eligible = 0L, n_events = 0L, risk = NA_real_)
  } else {
    # compute min time of any event in 'event'
    ft_event <- first_time_any(event)
    n_events <- vapply(times, function(t) sum(ft_event[eligible_run_ids] <= t), integer(1))
    res <- data.frame(
      time = times,
      n_eligible = rep.int(n_eligible, length(times)),
      n_events = n_events,
      risk = n_events / n_eligible
    )
  }

  spec <- list(
    event = event,
    times = times,
    start_time = start_time,
    terminal_events = terminal_events,
    condition_on_events = condition_on_events,
    eligible_provided = !is.null(eligible),
    denom = "fixed"
  )

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
  ifelse(is.na(x), NA, isTRUE(x))
}
