# ------------------------------------------------------------------------------
# Classes
# ------------------------------------------------------------------------------

#' @keywords internal
new_ps_forecast <- function(
  times,
  time0,
  run_index,
  defined,
  alive,
  vars,
  state,
  event_types,
  first_event_time,
  meta = list()
) {
  x <- list(
    times = as.numeric(times),
    time0 = as.numeric(time0),
    run_index = run_index,
    defined = defined,
    alive = alive,
    vars = as.character(vars),
    state = state,
    event_types = as.character(event_types),
    first_event_time = first_event_time,
    meta = meta
  )
  class(x) <- "ps_forecast"
  validate_ps_forecast(x)
  x
}

#' @keywords internal
validate_ps_forecast <- function(x) {
  if (!is.list(x)) stop("ps_forecast must be a list.", call. = FALSE)
  if (!is.numeric(x$times) || length(x$times) < 1L) stop("ps_forecast$times must be a non-empty numeric vector.", call. = FALSE)
  if (!is.numeric(x$time0) || length(x$time0) != 1L || !is.finite(x$time0)) stop("ps_forecast$time0 must be a finite numeric scalar.", call. = FALSE)
  if (!is.data.frame(x$run_index) || nrow(x$run_index) < 1L) stop("ps_forecast$run_index must be a non-empty data.frame.", call. = FALSE)

  R <- nrow(x$run_index)
  T <- length(x$times)

  if (!is.matrix(x$defined) || !is.logical(x$defined) || any(dim(x$defined) != c(R, T))) {
    stop("ps_forecast$defined must be a logical matrix with dim [n_runs x n_times].", call. = FALSE)
  }
  if (!is.matrix(x$alive) || !is.logical(x$alive) || any(dim(x$alive) != c(R, T))) {
    stop("ps_forecast$alive must be a logical matrix with dim [n_runs x n_times].", call. = FALSE)
  }
  # alive should be NA where defined is FALSE
  bad <- which(!x$defined & !is.na(x$alive), arr.ind = TRUE)
  if (nrow(bad) > 0) stop("ps_forecast$alive must be NA wherever defined is FALSE.", call. = FALSE)

  if (!is.character(x$vars) || length(x$vars) < 1L) stop("ps_forecast$vars must be a non-empty character vector.", call. = FALSE)
  if (!is.list(x$state) || !all(x$vars %in% names(x$state))) stop("ps_forecast$state must be a named list containing all vars.", call. = FALSE)

  for (v in x$vars) {
    m <- x$state[[v]]
    if (!is.matrix(m) || any(dim(m) != c(R, T))) {
      stop(sprintf("ps_forecast$state[['%s']] must be a matrix with dim [n_runs x n_times].", v), call. = FALSE)
    }
  }

  if (!is.character(x$event_types)) stop("ps_forecast$event_types must be a character vector.", call. = FALSE)
  if (!is.matrix(x$first_event_time) || !is.numeric(x$first_event_time) || nrow(x$first_event_time) != R) {
    stop("ps_forecast$first_event_time must be a numeric matrix with nrow = n_runs.", call. = FALSE)
  }
  if (length(x$event_types) != ncol(x$first_event_time)) {
    stop("Length of event_types must match ncol(first_event_time).", call. = FALSE)
  }

  # Schema metadata (required; stored sparsely once per forecast object).
  if (is.null(x$meta) || !is.list(x$meta)) stop("ps_forecast$meta must be a list.", call. = FALSE)
  schema <- x$meta$schema
  if (is.null(schema) || !is.list(schema) || is.null(names(schema)) || any(names(schema) == "")) {
    stop("ps_forecast$meta$schema must be a named list.", call. = FALSE)
  }
  allowed_types <- c("binary", "categorical", "ordinal", "continuous", "count")
  for (v in x$vars) {
    spec <- schema[[v]]
    if (is.null(spec) || !is.list(spec)) {
      stop(sprintf("ps_forecast$meta$schema is missing a spec for var '%s'.", v), call. = FALSE)
    }
    if (is.null(spec$type)) {
      stop(sprintf("schema spec for '%s' must define $type.", v), call. = FALSE)
    }
    tp <- as.character(spec$type)
    if (length(tp) != 1L || is.na(tp) || nchar(tp) == 0 || !(tp %in% allowed_types)) {
      stop(sprintf("schema spec for '%s' has invalid $type.", v), call. = FALSE)
    }
    if (tp %in% c("binary", "categorical", "ordinal")) {
      lev <- spec$levels
      if (is.null(lev) || !is.character(lev) || length(lev) < 2L || any(is.na(lev)) || any(lev == "")) {
        stop(sprintf("schema spec for '%s' must define $levels (character, length>=2) for type='%s'.", v, tp), call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}

#' @keywords internal
new_ps_risk <- function(spec, cohort, result, meta = list()) {
  x <- list(spec = spec, cohort = cohort, result = result, meta = meta)
  class(x) <- "ps_risk"
  validate_ps_risk(x)
  x
}

#' @keywords internal
validate_ps_risk <- function(x) {
  if (!is.list(x) || is.null(x$spec) || is.null(x$cohort) || is.null(x$result)) {
    stop("ps_risk must be a list with spec, cohort, and result.", call. = FALSE)
  }
  if (!is.list(x$spec)) stop("ps_risk$spec must be a list.", call. = FALSE)
  if (!is.list(x$cohort) || is.null(x$cohort$eligible_run_ids)) stop("ps_risk$cohort must include eligible_run_ids.", call. = FALSE)
  if (!is.data.frame(x$result) || !all(c("time", "n_eligible", "n_events", "risk") %in% names(x$result))) {
    stop("ps_risk$result must be a data.frame with columns time, n_eligible, n_events, risk.", call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Print methods
# ------------------------------------------------------------------------------

#' @export
print.ps_forecast <- function(x, ...) {
  idx <- x$run_index
  cat("<ps_forecast>\n")
  cat("  n_runs:", nrow(idx), "\n")
  cat("  times:", sprintf("[%s..%s] (%d)", min(x$times), max(x$times), length(x$times)), "\n")
  if (all(c("patient_id", "param_set_id", "sim_id") %in% names(idx))) {
    cat("  patients:", length(unique(idx$patient_id)), "\n")
    cat("  param_sets:", length(unique(idx$param_set_id)), "\n")
    cat("  sims_per_combo:", length(unique(idx$sim_id)), "\n")
  }
  cat("  vars:", paste(x$vars, collapse = ", "), "\n")
  cat("  event_types:", length(x$event_types), "\n")
  invisible(x)
}

#' @export
print.ps_risk <- function(x, ...) {
  cat("<ps_risk>\n")
  ev <- x$spec$event
  cat("  event:", paste(ev, collapse = ", "), "\n")
  cat("  start_time:", x$spec$start_time, "\n")
  cat("  n_eligible:", x$cohort$n_eligible, "\n")
  cat("  times:", sprintf("[%s..%s] (%d)", min(x$result$time), max(x$result$time), nrow(x$result)), "\n")
  invisible(x)
}
