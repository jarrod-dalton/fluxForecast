# ------------------------------------------------------------------------------
# Classes
# ------------------------------------------------------------------------------

new_forecast <- function(
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
    times = .fluxf_as_numeric_time(times, name = "times"),
    time0 = .fluxf_as_numeric_time(time0, name = "time0"),
    run_index = run_index,
    defined = defined,
    alive = alive,
    vars = as.character(vars),
    state = state,
    event_types = as.character(event_types),
    first_event_time = first_event_time,
    meta = meta
  )
  class(x) <- "flux_forecast"
  validate_forecast(x)
  x
}

validate_forecast <- function(x) {
  if (!is.list(x)) stop("flux_forecast must be a list.", call. = FALSE)
  if (!is.numeric(x$times) || length(x$times) < 1L) stop("flux_forecast$times must be a non-empty numeric vector.", call. = FALSE)
  if (!is.numeric(x$time0) || length(x$time0) != 1L || !is.finite(x$time0)) stop("flux_forecast$time0 must be a finite numeric scalar.", call. = FALSE)
  if (!is.data.frame(x$run_index) || nrow(x$run_index) < 1L) stop("flux_forecast$run_index must be a non-empty data.frame.", call. = FALSE)

  R <- nrow(x$run_index)
  T <- length(x$times)

  if (!is.matrix(x$defined) || !is.logical(x$defined) || any(dim(x$defined) != c(R, T))) {
    stop("flux_forecast$defined must be a logical matrix with dim [n_runs x n_times].", call. = FALSE)
  }
  if (!is.matrix(x$alive) || !is.logical(x$alive) || any(dim(x$alive) != c(R, T))) {
    stop("flux_forecast$alive must be a logical matrix with dim [n_runs x n_times].", call. = FALSE)
  }
  # alive should be NA where defined is FALSE
  bad <- which(!x$defined & !is.na(x$alive), arr.ind = TRUE)
  if (nrow(bad) > 0) stop("flux_forecast$alive must be NA wherever defined is FALSE.", call. = FALSE)

  if (!is.character(x$vars) || length(x$vars) < 1L) stop("flux_forecast$vars must be a non-empty character vector.", call. = FALSE)
  if (!is.list(x$state) || !all(x$vars %in% names(x$state))) stop("flux_forecast$state must be a named list containing all vars.", call. = FALSE)

  for (v in x$vars) {
    m <- x$state[[v]]
    if (!is.matrix(m) || any(dim(m) != c(R, T))) {
      stop(sprintf("flux_forecast$state[['%s']] must be a matrix with dim [n_runs x n_times].", v), call. = FALSE)
    }
  }

  if (!is.character(x$event_types)) stop("flux_forecast$event_types must be a character vector.", call. = FALSE)
  if (!is.matrix(x$first_event_time) || !is.numeric(x$first_event_time) || nrow(x$first_event_time) != R) {
    stop("flux_forecast$first_event_time must be a numeric matrix with nrow = n_runs.", call. = FALSE)
  }
  if (length(x$event_types) != ncol(x$first_event_time)) {
    stop("Length of event_types must match ncol(first_event_time).", call. = FALSE)
  }

  # Schema metadata (required; stored sparsely once per forecast object).
  if (is.null(x$meta) || !is.list(x$meta)) stop("flux_forecast$meta must be a list.", call. = FALSE)
  schema <- x$meta$schema
  if (is.null(schema) || !is.list(schema) || is.null(names(schema)) || any(names(schema) == "")) {
    stop("flux_forecast$meta$schema must be a named list.", call. = FALSE)
  }
  # Delegate full schema validation (including type allow-list and levels
  # checks) to fluxCore — single source of truth.
  fluxCore::schema_validate(schema)
  for (v in x$vars) {
    if (is.null(schema[[v]])) {
      stop(sprintf("flux_forecast$meta$schema is missing a spec for var '%s'.", v), call. = FALSE)
    }
  }

  invisible(TRUE)
}

new_event_prob <- function(spec, cohort, result, meta = list()) {
  x <- list(spec = spec, cohort = cohort, result = result, meta = meta)
  class(x) <- "flux_event_prob"
  validate_event_prob(x)
  x
}

validate_event_prob <- function(x) {
  if (!is.list(x) || is.null(x$spec) || is.null(x$cohort) || is.null(x$result)) {
    stop("flux_event_prob must be a list with spec, cohort, and result.", call. = FALSE)
  }
  if (!is.list(x$spec)) stop("flux_event_prob$spec must be a list.", call. = FALSE)
  if (!is.list(x$cohort) || is.null(x$cohort$eligible_run_ids)) stop("flux_event_prob$cohort must include eligible_run_ids.", call. = FALSE)
  if (!is.data.frame(x$result) || !all(c("time", "n_eligible", "n_events", "event_prob") %in% names(x$result))) {
    stop("flux_event_prob$result must be a data.frame with columns time, n_eligible, n_events, event_prob.", call. = FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Print methods
# ------------------------------------------------------------------------------

print.flux_forecast <- function(x, ...) {
  idx <- x$run_index
  cat("<flux_forecast>\n")
  cat("  n_runs:", nrow(idx), "\n")
  cat("  times:", sprintf("[%s..%s] (%d)", min(x$times), max(x$times), length(x$times)), "\n")
  if (all(c("entity_id", "param_draw_id", "sim_id") %in% names(idx))) {
    cat("  entities:", length(unique(idx$entity_id)), "\n")
    cat("  param_draws:", length(unique(idx$param_draw_id)), "\n")
    cat("  sims_per_combo:", length(unique(idx$sim_id)), "\n")
  }
  cat("  vars:", paste(x$vars, collapse = ", "), "\n")
  cat("  event_types:", length(x$event_types), "\n")
  invisible(x)
}

print.flux_event_prob <- function(x, ...) {
  cat("<flux_event_prob>\n")
  ev <- x$spec$event
  cat("  event:", paste(ev, collapse = ", "), "\n")
  cat("  start_time:", x$spec$start_time, "\n")
  cat("  n_eligible:", x$cohort$n_eligible, "\n")
  cat("  times:", sprintf("[%s..%s] (%d)", min(x$result$time), max(x$result$time), nrow(x$result)), "\n")
  invisible(x)
}
