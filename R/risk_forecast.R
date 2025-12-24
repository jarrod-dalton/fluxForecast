# ------------------------------------------------------------------------------
# risk_forecast(): streaming-style risk estimation without materializing ps_forecast
# ------------------------------------------------------------------------------

#' Compute event risk curves directly from forward simulations
#'
#' This helper runs simulations and returns a ps_risk object without retaining
#' per-run state matrices. It is designed to be memory-friendly for large jobs.
#'
#' @param engine A patientSimCore Engine.
#' @param patients A list of Patient objects (or a single Patient).
#' @param times Numeric vector of forecast times.
#' @param S Integer simulations per (patient, param_set).
#' @param param_sets Optional list of parameter lists.
#' @param event Character vector of event types of interest.
#' @param start_time Optional scalar time in `times` at which to define the eligible cohort.
#'   Default is min(times).
#' @param terminal_events Optional event types that must be absent by start_time.
#' @param condition_on_events Optional event types that must be absent by start_time.
#' @param eligible Optional function(snapshot, time, ctx) -> TRUE/FALSE. Evaluated at start_time only.
#' @param eligible_vars Character vector of snapshot variables to pass to `eligible()`.
#'   'alive' is always included.
#' @param ctx Optional list passed to `eligible()`.
#' @param max_events Passed to Engine$run().
#' @param seed Optional base seed.
#' @param parallel Logical; if TRUE uses patientSimCore::run_cohort(parallel=TRUE).
#' @param n_workers Optional number of workers.
#' @param ctx_base Optional ctx fields (besides params) to pass to Engine$run().
#'
#' @return A ps_risk object.
#' @export
risk_forecast <- function(
  engine,
  patients,
  times,
  S = 200,
  param_sets = NULL,
  event,
  start_time = NULL,
  terminal_events = NULL,
  condition_on_events = NULL,
  eligible = NULL,
  eligible_vars = NULL,
  ctx = NULL,
  max_events = 1000,
  seed = NULL,
  parallel = FALSE,
  n_workers = NULL,
  ctx_base = NULL
) {
  if (missing(engine) || is.null(engine)) stop("engine is required.", call. = FALSE)
  if (!inherits(engine, "Engine")) stop("engine must be a patientSimCore::Engine.", call. = FALSE)

  if (inherits(patients, "Patient")) patients <- list(p1 = patients)
  if (!is.list(patients) || length(patients) == 0L) stop("patients must be a non-empty list of Patient objects.", call. = FALSE)

  times <- as.numeric(times)
  if (length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)
  times <- sort(unique(times))

  S <- as.integer(S)
  if (!is.finite(S) || S < 1L) stop("S must be a positive integer.", call. = FALSE)

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }
  P <- length(param_sets)

  event <- unique(as.character(event))
  if (length(event) < 1L) stop("event must be a non-empty character vector.", call. = FALSE)

  if (is.null(start_time)) start_time <- min(times)
  start_time <- as.numeric(start_time)
  if (!is.finite(start_time) || length(start_time) != 1L) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be one of the provided times (v1 restriction).", call. = FALSE)

  if (!is.null(eligible) && !is.function(eligible)) stop("eligible must be a function if provided.", call. = FALSE)
  if (!is.null(ctx) && !is.list(ctx)) stop("ctx must be a list or NULL.", call. = FALSE)
  if (!is.null(ctx_base) && !is.list(ctx_base)) stop("ctx_base must be a list or NULL.", call. = FALSE)

  # Variables needed for eligibility predicate
  if (is.null(eligible_vars)) {
    eligible_vars <- "alive"
  } else {
    eligible_vars <- unique(as.character(eligible_vars))
    eligible_vars <- unique(c(eligible_vars, "alive"))
  }

  # Event sets we may need to inspect
  needed_events <- unique(c(event, terminal_events, condition_on_events))
  needed_events <- needed_events[!is.na(needed_events) & nzchar(needed_events)]

  horizon <- max(times)

  # We chunk over simulations per call to keep peak memory down.
  # Chunk size is chosen to be modest; users can still parallelize within run_cohort.
  chunk_size <- min(100L, S)
  n_chunks <- ceiling(S / chunk_size)

  # Accumulators
  T <- length(times)
  n_events_by_time <- integer(T)
  n_eligible_total <- 0L

  # helper: min time of needed events in a single run event table
  min_time_any <- function(ev_df, ev_set) {
    if (is.null(ev_df) || nrow(ev_df) == 0L) return(Inf)
    w <- ev_df$event_type %in% ev_set
    if (!any(w)) return(Inf)
    min(ev_df$time[w])
  }

  for (cc in seq_len(n_chunks)) {
    s0 <- (cc - 1L) * chunk_size + 1L
    s1 <- min(S, cc * chunk_size)
    n_sims_chunk <- s1 - s0 + 1L

    out <- patientSimCore::run_cohort(
      engine = engine,
      patients = patients,
      n_param_draws = P,
      n_sims = n_sims_chunk,
      param_draws = param_sets,
      max_events = max_events,
      max_time = horizon,
      return_observations = FALSE,
      parallel = isTRUE(parallel),
      n_workers = n_workers,
      seed = if (is.null(seed)) NULL else as.integer(seed) + (cc - 1L) * 100000L
    )

    runs <- out$runs

    for (r in runs) {
      p <- r$patient
      if (is.null(p)) next

      # defined at start_time?
      if (start_time > p$last_time) next

      snap <- p$snapshot_at_time(start_time, vars = eligible_vars)
      if (is.null(snap[["alive"]])) {
        stop("snapshot_at_time() did not return an 'alive' field. Ensure schema includes 'alive' (logical).", call. = FALSE)
      }
      if (!isTRUE(snap[["alive"]])) next

      ev <- r$events

      # event-free requirements at start_time
      if (!is.null(terminal_events)) {
        if (min_time_any(ev, terminal_events) <= start_time) next
      }
      if (!is.null(condition_on_events)) {
        if (min_time_any(ev, condition_on_events) <= start_time) next
      }

      # predicate
      if (!is.null(eligible)) {
        if (!isTRUE(eligible(snap, start_time, ctx))) next
      }

      # eligible
      n_eligible_total <- n_eligible_total + 1L

      t_ev <- min_time_any(ev, event)
      if (!is.finite(t_ev)) next
      n_events_by_time <- n_events_by_time + as.integer(t_ev <= times)
    }
  }

  res <- if (n_eligible_total == 0L) {
    data.frame(time = times, n_eligible = 0L, n_events = 0L, risk = NA_real_)
  } else {
    data.frame(
      time = times,
      n_eligible = rep.int(n_eligible_total, length(times)),
      n_events = n_events_by_time,
      risk = n_events_by_time / n_eligible_total
    )
  }

  spec <- list(
    event = event,
    times = times,
    start_time = start_time,
    terminal_events = terminal_events,
    condition_on_events = condition_on_events,
    eligible_provided = !is.null(eligible),
    denom = "fixed",
    streaming = TRUE
  )

  cohort <- list(
    eligible_run_ids = integer(0),
    n_eligible = n_eligible_total
  )

  new_ps_risk(spec = spec, cohort = cohort, result = res)
}
