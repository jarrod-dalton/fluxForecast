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
#' @param parallel Logical; if TRUE uses patientSimCore::run_cohort(parallel=TRUE).
#' @param n_workers Optional number of workers (passed to run_cohort).
#' @param return One of c('object','summary_stats','none').
#' @param summary_stats If return='summary_stats', a character vector of summaries to compute.
#'   Currently supports 'risk' only, via a spec list passed in meta$summary_spec.
#' @param ctx_base Optional list of ctx fields to pass to Engine$run() besides params.
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
  parallel = FALSE,
  n_workers = NULL,
  return = c("object", "summary_stats", "none"),
  summary_stats = NULL,
  summary_spec = NULL,
  ctx_base = NULL
) {
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

  horizon <- max(times)

  # Build wrapper ctx that is merged in per-run inside run_cohort (via ctx$params)
  # We use ctx_base by injecting into Engine$run via ctx within run_cohort.
  if (!is.null(ctx_base) && !is.list(ctx_base)) stop("ctx_base must be a list or NULL.", call. = FALSE)

  # --------------------------------------------------------------------------
  # summary_stats short-circuit: compute requested summaries without retaining
  # full per-run matrices (ps_forecast). This is the memory-friendly mode.
  # --------------------------------------------------------------------------
  if (return == "summary_stats") {
    if (is.null(summary_stats)) stop("summary_stats must be specified when return='summary_stats'.", call. = FALSE)
    summary_stats <- unique(as.character(summary_stats))

    # Allow summary_spec in either the explicit argument or ctx_base$summary_spec
    if (is.null(summary_spec) && is.list(ctx_base) && !is.null(ctx_base$summary_spec)) {
      summary_spec <- ctx_base$summary_spec
    }

    res <- list()
    for (s in summary_stats) {
      if (identical(s, "risk")) {
        if (is.null(summary_spec) || !is.list(summary_spec)) {
          stop("For summary_stats='risk', provide summary_spec as a list (e.g., list(event=..., start_time=..., terminal_events=...)).", call. = FALSE)
        }
        res[["risk"]] <- do.call(
          risk_forecast,
          c(
            list(
              engine = engine,
              patients = patients,
              times = times,
              S = S,
              param_sets = param_sets,
              max_events = max_events,
              seed = seed,
              parallel = parallel,
              n_workers = n_workers,
              ctx_base = ctx_base
            ),
            summary_spec
          )
        )
      } else {
        stop("Unknown summary_stats: ", s, call. = FALSE)
      }
    }
    return(res)
  }

  # Use patientSimCore::run_cohort to run R = N * P * S simulations.
  out <- patientSimCore::run_cohort(
    engine = engine,
    patients = patients,
    n_param_draws = P,
    n_sims = S,
    param_draws = param_sets,
    max_events = max_events,
    max_time = horizon,
    return_observations = FALSE,
    parallel = isTRUE(parallel),
    n_workers = n_workers,
    seed = seed
  )

  runs <- out$runs
  idx <- out$index

  # Map patient ids to integer IDs for compactness
  patient_levels <- unique(idx$patient_id)
  patient_id_int <- match(idx$patient_id, patient_levels)

  run_index <- data.frame(
    run_id = idx$run_id,
    patient_id = patient_id_int,
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
  state <- setNames(vector("list", length(vars)), vars)
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
        if (!is.na(j)) first_event_time[i, j] <- min(by_type[[et]])
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
        alive[i, tt] <- isTRUE(snap[["alive"]])

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
    ctx_base = ctx_base,
    seed = seed,
    S = S,
    P = P
  )

  if (return == "none") return(invisible(NULL))

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

  if (return == "object") return(x)

  # return == 'none'
  invisible(NULL)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
