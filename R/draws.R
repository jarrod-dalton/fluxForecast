# ------------------------------------------------------------------------------
# draws()
# ------------------------------------------------------------------------------

#' Extract simulated draws
#'
#' Extract per-run draws for one or more variables from a `flux_forecast`.
#'
#' @param x A `flux_forecast`.
#' @param var Character vector of variables to extract.
#' @param times Optional subset of forecast times.
#' @param start_time Start time used to define a fixed cohort when
#'   `cohort = "fixed"`.
#' @param eligible Optional function `f(snapshot, time, ctx)` evaluated at
#'   `start_time` only.
#' @param ctx Optional list passed to `eligible`.
#' @param cohort `"timepoint"` or `"fixed"`.
#'
#' @return A data.frame when a single variable is requested; otherwise a named
#'   list of data.frames.
#' @export
draws <- function(
  x,
  var,
  times = NULL,
  start_time = NULL,
  eligible = NULL,
  ctx = NULL,
  cohort = c("timepoint", "fixed")
) {
  if (!inherits(x, "flux_forecast")) stop("x must be a flux_forecast.", call. = FALSE)
  cohort <- match.arg(cohort)

  var <- unique(as.character(var))
  if (length(var) < 1L) stop("var must be a non-empty character vector.", call. = FALSE)
  if (!all(var %in% x$vars)) {
    missing_vars <- var[!var %in% x$vars]
    stop("Unknown var(s) not stored in flux_forecast: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  if (is.null(times)) {
    times <- x$times
  } else {
    times <- sort(unique(.fluxf_as_numeric_time(times, name = "times", ctx = ctx, time_spec = x$meta$time_spec)))
    if (!all(times %in% x$times)) stop("All times must be members of x$times.", call. = FALSE)
  }
  t_idx <- match(times, x$times)

  if (is.null(start_time)) start_time <- x$time0
  start_time <- .fluxf_as_numeric_time(start_time, name = "start_time", ctx = ctx, time_spec = x$meta$time_spec)
  if (!start_time %in% x$times) stop("start_time must be one of x$times (v1 restriction).", call. = FALSE)
  t_start_idx <- match(start_time, x$times)

  # Determine cohort run_ids if needed
  fixed_ids <- NULL
  if (cohort == "fixed" || !is.null(eligible)) {
    alive0 <- x$alive[, t_start_idx]
    if (is.character(alive0)) alive0 <- as.logical(alive0)
    elig <- (x$defined[, t_start_idx] %in% TRUE) & (alive0 %in% TRUE)

    if (!is.null(eligible)) {
      if (!is.function(eligible)) stop("eligible must be a function if provided.", call. = FALSE)
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
    fixed_ids <- which(elig)
  }

  build_one <- function(v) {
    m <- x$state[[v]]

    rows <- list()
    idx <- 0L

    for (j in seq_along(t_idx)) {
      tt <- t_idx[[j]]
      tval <- times[[j]]

      if (cohort == "fixed") {
        run_ids <- fixed_ids
        if (length(run_ids) == 0L) next
        # only include defined at this time
        run_ids <- run_ids[x$defined[run_ids, tt] %in% TRUE]
        if (length(run_ids) == 0L) next
      } else {
        # timepoint cohort: defined & alive at each time
        alive_t <- x$alive[, tt]
        if (is.character(alive_t)) alive_t <- as.logical(alive_t)
        run_ids <- which((x$defined[, tt] %in% TRUE) & (alive_t %in% TRUE))
        if (length(run_ids) == 0L) next
      }

      vals <- m[run_ids, tt]
      # drop NAs
      ok <- !is.na(vals)
      run_ids <- run_ids[ok]
      vals <- vals[ok]
      if (length(vals) == 0L) next

      idx <- idx + 1L
      rows[[idx]] <- data.frame(
        run_id = x$run_index$run_id[run_ids],
        time = rep.int(tval, length(vals)),
        value = vals,
        stringsAsFactors = FALSE
      )
    }

    if (length(rows) == 0L) {
      data.frame(run_id = character(0), time = numeric(0), value = vector(mode = "list", length = 0))
    } else {
      do.call(rbind, rows)
    }
  }

  if (length(var) == 1L) return(build_one(var[[1]]))

  out <- stats::setNames(vector("list", length(var)), var)
  for (k in seq_along(var)) out[[k]] <- build_one(var[[k]])
  out
}
