# ------------------------------------------------------------------------------
# forecast()
# ------------------------------------------------------------------------------

#' Run forward-simulation forecasts for one or more patients
#'
#' @param engine A patientSimCore Engine.
#' @param patients A list of patientSimCore Patient objects (or a single Patient).
#' @param times Numeric vector of forecast times (must be >= patient time0).
#' @param S Integer simulations per (patient, param_set).
#' @param param_sets Optional list of parameter lists. If NULL, uses engine defaults.
#' @param vars Character vector of snapshot variables to store. 'alive' is always included.
#' @param max_events Passed to Engine$run().
#' @param seed Optional base seed for deterministic per-run seeds.
#' @param backend Parallel backend. One of c('none','mclapply','cluster','future').
#'   - 'none': run serially.
#'   - 'mclapply': use parallel::mclapply (macOS/Linux; not Windows).
#'   - 'cluster': use a PSOCK cluster via patientSimCore::run_cohort(backend='cluster').
#'   - 'future': use future.apply::future_lapply via patientSimCore::run_cohort(backend='future').
#' @param n_workers Optional number of workers.
#' @param return One of c('object','summary_stats','none').
#' @param summary_stats If return='summary_stats', a character vector of summaries to compute.
#'   Currently supports 'risk' and 'state'.
#' @param summary_spec If return='summary_stats', a named list describing the summary to compute.
#'   For summary_stats='risk', this is passed to risk_forecast().
#'   For summary_stats='state', this is passed to state_summary_forecast().
#' @param ctx Optional simulation context. Can be either:
#'   - a single list merged into ctx for every run, or
#'   - a list of per-parameter-set ctx lists (length equal to length(param_sets)).
#'     Each per-draw ctx may include its own $params; if so, that overrides param_sets.
#'
#' @return A ps_forecast object if return='object'. If return='summary_stats', returns a list
#'   of summary objects. If return='none', returns invisible(NULL).
#'
#' @export
forecast <- function(
  engine,
  patients,
  times,
  S = 200,
  param_sets = NULL,
  vars = NULL,
  max_events = 1000,
  seed = NULL,
  backend = c("none", "mclapply", "cluster", "future"),
  n_workers = NULL,
  return = c("object", "summary_stats", "none"),
  summary_stats = c("both", "risk", "state"),
  summary_spec = NULL,
  ctx = NULL
) {
  backend <- match.arg(backend)
  return <- match.arg(return)

  if (missing(engine) || is.null(engine)) stop("engine is required.", call. = FALSE)
  if (!inherits(engine, "Engine")) stop("engine must be a patientSimCore::Engine.", call. = FALSE)

  if (inherits(patients, "Patient")) patients <- list(p1 = patients)
  if (!is.list(patients) || length(patients) == 0L) stop("patients must be a non-empty list of Patient objects.", call. = FALSE)

  times <- as.numeric(times)
  if (!is.numeric(times) || length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)
  times <- sort(unique(times))

  S <- as.integer(S)
  if (!is.finite(S) || S < 1L) stop("S must be a positive integer.", call. = FALSE)

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }
  P <- length(param_sets)

  if (is.null(vars)) {
    vars <- "alive"
  } else {
    vars <- unique(as.character(vars))
    vars <- unique(c(vars, "alive"))
  }

  # Capture and validate schema metadata (sparse, stored once per forecast object).
  # We require schemas to be identical across patients within a single forecast call.
  schema0 <- patients[[1]]$schema
  if (is.null(schema0) || !is.list(schema0) || is.null(names(schema0))) {
    stop("patients[[1]] is missing a valid $schema.", call. = FALSE)
  }
  for (nm in names(patients)) {
    sc <- patients[[nm]]$schema
    if (!identical(sc, schema0)) {
      stop("All patients passed to forecast() must share an identical schema.", call. = FALSE)
    }
  }

  horizon <- max(times)

  if (!is.null(ctx) && !is.list(ctx)) stop("ctx must be a list, a list of lists, or NULL.", call. = FALSE)

  # Use patientSimCore::run_cohort to run R = N * P * S simulations.

  out <- suppressWarnings(patientSimCore::run_cohort(
    engine = engine,
    patients = patients,
    n_param_draws = P,
    n_sims = S,
    param_draws = param_sets,
    ctx = ctx,
    max_events = max_events,
    max_time = horizon,
    return_observations = FALSE,
    backend = backend,
    n_workers = n_workers,
    seed = seed
  ))

  runs <- out$runs
  idx <- out$index

  # Core guarantees that runs[[i]] corresponds to idx[i, ] (run_index alignment
  # invariant). Forecast can therefore populate matrices directly in run order.

  # Map patient ids to integer IDs for compactness
  patient_levels <- unique(idx$patient_id)
  patient_id_int <- match(idx$patient_id, patient_levels)

  patient_tags <- vapply(patient_levels, function(pid) {
    p <- patients[[pid]]
    if (is.null(p)) return(NA_character_)
    if (!is.null(p$id)) as.character(p$id) else NA_character_
  }, character(1))

  run_index <- data.frame(
    run_id = idx$run_id,
    patient_id = patient_id_int,
    patient_tag = patient_tags[patient_id_int],
    # Canonical naming for parameter draws in the ecosystem is draw_id.
    draw_id = idx$draw_id,
    # Back-compat alias (used in some early streaming code).
    param_set_id = idx$draw_id,
    sim_id = idx$sim_id,
    stringsAsFactors = FALSE
  )

  # Build matrices
  R <- nrow(run_index)
  T <- length(times)

  defined <- matrix(FALSE, nrow = R, ncol = T)
  alive <- matrix(NA, nrow = R, ncol = T)

  # State matrices per var
  state <- stats::setNames(vector("list", length(vars)), vars)
  for (v in vars) {
    state[[v]] <- matrix(NA, nrow = R, ncol = T)
  }

  # Collect event types across runs
  # (In large jobs this can be big; we keep it simple in v1.)
  all_event_types <- unique(unlist(lapply(runs, function(r) unique(as.character(r$events$event_type)))))
  all_event_types <- sort(all_event_types)
  K <- length(all_event_types)
  first_event_time <- matrix(Inf, nrow = R, ncol = K)
  colnames(first_event_time) <- all_event_types

  # Populate matrices
  for (i in seq_len(R)) {
    r <- runs[[i]]
    p <- r$patient
    last_time <- p$last_time

    # first event time per type
    ev <- r$events
    if (!is.null(ev) && nrow(ev) > 0) {
      # Split by event_type
      by_type <- split(ev$time, ev$event_type)
      for (et in names(by_type)) {
        j <- match(et, all_event_types)
        # Keep Inf when an event type never occurs; guard against NA times.
        if (!is.na(j)) {
          tmin <- suppressWarnings(min(by_type[[et]], na.rm = TRUE))
          if (is.finite(tmin)) first_event_time[i, j] <- tmin
        }
      }
    }

    for (tt in seq_along(times)) {
      t <- times[[tt]]
      if (t <= last_time) {
        defined[i, tt] <- TRUE
        snap <- p$snapshot_at_time(t, vars = vars)

        if (is.null(snap[["alive"]])) {
          stop("snapshot_at_time() did not return an 'alive' field. Ensure schema includes 'alive' (logical).", call. = FALSE)
        }
        # Preserve NA if a model ever represents unknown vital status at a defined time.
        # (Core v1.0 enforces alive is non-NA, but downstream code should be robust.)
        av <- snap[["alive"]]
        alive[i, tt] <- ps_na_safe_true1(av)

        for (v in vars) {
          val <- snap[[v]]
          # Store non-numeric as character in the matrix
          if (!is.null(val) && !is.numeric(val) && !is.logical(val)) {
            state[[v]][i, tt] <- as.character(val)
          } else {
            state[[v]][i, tt] <- val
          }
        }
      } else {
        defined[i, tt] <- FALSE
        alive[i, tt] <- NA
        # state remains NA
      }
    }
  }

  meta <- list(
    patient_levels = patient_levels,
    patient_tags = patient_tags,
    schema = schema0,
    ctx = ctx,
    seed = seed,
    S = S,
    P = P
  )

  if (return == "none") return(invisible(NULL))

  # Memory-light summaries (do not materialize a ps_forecast).
  


  x <- new_ps_forecast(
    times = times,
    time0 = min(times),
    run_index = run_index,
    defined = defined,
    alive = alive,
    vars = vars,
    state = state,
    event_types = all_event_types,
    first_event_time = first_event_time,
    meta = meta
  )

  
if (return == "summary_stats") {
  summary_stats <- match.arg(summary_stats, c("both", "risk", "state"))

  res <- list()

  if (summary_stats %in% c("risk", "both")) {
    if (is.null(summary_spec)) stop("summary_spec must be provided when summary_stats includes 'risk'.", call. = FALSE)
    res$risk <- do.call(risk, c(list(x = x), summary_spec))
  }

  if (summary_stats %in% c("state", "both")) {
    res$state <- state_summary(x, vars = vars, times = times)
  }

  if (summary_stats == "risk") return(res$risk)
  if (summary_stats == "state") return(res$state)
  return(res)
}

if (return == "object") return(x)

  x
}

`%||%` <- function(x, y) if (is.null(x)) y else x
