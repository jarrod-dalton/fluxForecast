# ------------------------------------------------------------------------------
# forecast()
# ------------------------------------------------------------------------------

#' Run forward-simulation forecasts for one or more entities
#'
#' Runs forward simulations using a fluxCore Engine and collects snapshots at the
#' requested times. If models expose an `alive` state variable, it is used for
#' lifecycle eligibility. If `alive` is omitted, lifecycle status is derived from
#' `bundle$terminal_events` when declared, otherwise it defaults to active
#' wherever runs are defined.
#'
#' @param engine A fluxCore Engine.
#' @param entities A list of Entity objects (or a single Entity).
#' @param times Numeric vector of forecast times.
#' @param S Number of simulations per (entity, param_set).
#' @param param_sets Optional list of parameter lists.
#' @param vars Snapshot variables to store. If schema includes `alive`, it is
#'   always included.
#' @param max_events Maximum number of events per run.
#' @param seed Optional base seed.
#' @param backend Parallel backend. One of `c("none", "mclapply", "cluster", "future")`.
#' @param n_workers Optional number of workers. For `backend = "future"`, set
#'   workers via `future::plan()`.
#' @param return One of `"object"`, `"summary_stats"`, or `"none"`.
#' @param summary_stats Summary name when `return = "summary_stats"` (v1 supports
#'   `"event_prob"` or `"state"`).
#' @param summary_spec Named list describing the summary to compute when
#'   `return = "summary_stats"`. For `summary_stats = "event_prob"` this is
#'   passed to `event_prob_forecast()`. For `summary_stats = "state"` this is
#'   passed to `state_summary_forecast()`.
#' @param ctx Optional simulation context. Either a single list merged into each
#'   run ctx, or a list of per-parameter-set ctx lists. Each per-draw ctx may
#'   include its own `$params`; if so, that overrides `param_sets`.
#'
#' @return A `flux_forecast` object when `return = "object"`.
#' @export
forecast <- function(
  engine,
  entities,
  times,
  S = 200,
  param_sets = NULL,
  vars = NULL,
  max_events = 1000,
  seed = NULL,
  backend = c("none", "mclapply", "cluster", "future"),
  n_workers = NULL,
  return = c("object", "summary_stats", "none"),
  summary_stats = c("both", "event_prob", "state"),
  summary_spec = NULL,
  ctx = NULL
) {
  backend <- match.arg(backend)
  return <- match.arg(return)

  if (missing(engine) || is.null(engine)) stop("engine is required.", call. = FALSE)
  if (!inherits(engine, "Engine")) stop("engine must be a fluxCore::Engine.", call. = FALSE)

  if (inherits(entities, "Entity")) entities <- list(p1 = entities)
  if (!is.list(entities) || length(entities) == 0L) stop("entities must be a non-empty list of Entity objects.", call. = FALSE)

  times <- .fluxf_as_numeric_time(times, name = "times", time_spec = engine$time_spec)
  if (!is.numeric(times) || length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)
  times <- sort(unique(times))

  # Guard: ctx$time is a v1.x pattern and must not be used to override canonical time spec.
  if (!is.null(ctx) && is.list(ctx) && !is.null(ctx$time)) {
    stop(
      "forecast(): cannot override canonical model time spec via ctx$time. ",
      "The time spec is fixed by the engine's bundle$time_spec.",
      call. = FALSE
    )
  }

  S <- as.integer(S)
  if (!is.finite(S) || S < 1L) stop("S must be a positive integer.", call. = FALSE)

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }
  P <- length(param_sets)

  # Capture and validate schema metadata (sparse, stored once per forecast object).
  # We require schemas to be identical across entities within a single forecast call.
  schema0 <- entities[[1]]$schema
  if (is.null(schema0) || !is.list(schema0) || is.null(names(schema0))) {
    stop("entities[[1]] is missing a valid $schema.", call. = FALSE)
  }
  for (nm in names(entities)) {
    sc <- entities[[nm]]$schema
    if (!identical(sc, schema0)) {
      stop("All entities passed to forecast() must share an identical schema.", call. = FALSE)
    }
  }
  has_alive_schema <- "alive" %in% names(schema0)

  model_event_catalog <- .fluxf_bundle_event_catalog(engine$bundle)
  model_terminal_events <- .fluxf_bundle_terminal_events(engine$bundle)

  if (is.null(vars)) {
    if (has_alive_schema) {
      vars <- "alive"
    } else {
      schema_vars <- names(schema0)
      if (length(schema_vars) < 1L) {
        stop("schema must define at least one state variable when 'alive' is not present.", call. = FALSE)
      }
      vars <- schema_vars[1]
    }
  } else {
    vars <- unique(as.character(vars))
  }

  if (has_alive_schema) {
    vars <- unique(c(vars, "alive"))
  } else if ("alive" %in% vars) {
    stop("vars includes 'alive', but the model schema does not define an 'alive' state variable.", call. = FALSE)
  }

  if (length(vars) < 1L) {
    stop("vars must contain at least one state variable.", call. = FALSE)
  }

  if (!has_alive_schema) {
    if (!is.null(model_terminal_events)) {
      .fluxf_warn_once(
        "forecast_lifecycle_terminal_fallback",
        "Model schema omits 'alive'; deriving lifecycle status from bundle$terminal_events."
      )
    } else {
      .fluxf_warn_once(
        "forecast_lifecycle_defined_fallback",
        "Model schema omits 'alive' and bundle$terminal_events is not set; treating lifecycle status as active wherever runs are defined."
      )
    }
  }

  horizon <- max(times)

  # Use fluxCore::run_cohort to run R = N * P * S simulations.

  out <- suppressWarnings(fluxCore::run_cohort(
    engine = engine,
    entities = entities,
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

  # Map entity ids to integer IDs for compactness
  entity_levels <- unique(idx$entity_id)
  entity_id_int <- match(idx$entity_id, entity_levels)

  entity_tags <- vapply(entity_levels, function(pid) {
    p <- entities[[pid]]
    if (is.null(p)) return(NA_character_)
    if (!is.null(p$id)) as.character(p$id) else NA_character_
  }, character(1))

  run_index <- data.frame(
    run_id = idx$run_id,
    entity_id = entity_id_int,
    entity_tag = entity_tags[entity_id_int],
    # Canonical naming for parameter draws in the ecosystem is param_draw_id.
    param_draw_id = idx$param_draw_id,
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

  if (!has_alive_schema && !is.null(model_terminal_events) && is.null(model_event_catalog)) {
    unseen <- setdiff(model_terminal_events, all_event_types)
    if (length(unseen) > 0L) {
      .fluxf_warn_once(
        "forecast_terminal_unseen_without_catalog",
        paste0(
          "bundle$terminal_events contains labels not observed in this forecast run (and no bundle$event_catalog is declared): ",
          paste(unseen, collapse = ", ")
        )
      )
    }
  }

  # Populate matrices
  for (i in seq_len(R)) {
    r <- runs[[i]]
    p <- r$entity
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
    terminal_time <- .fluxf_first_event_time_any(ev, model_terminal_events)

    for (tt in seq_along(times)) {
      t <- times[[tt]]
      if (t <= last_time) {
        defined[i, tt] <- TRUE
        snap <- p$snapshot_at_time(t, vars = vars)

        if (has_alive_schema) {
          if (is.null(snap[["alive"]])) {
            stop("snapshot_at_time() did not return an 'alive' field. Ensure schema includes 'alive' (logical).", call. = FALSE)
          }
          # Preserve NA if a model ever represents unknown lifecycle status at a defined time.
          av <- norm_bool(snap[["alive"]])
          alive[i, tt] <- na_safe_true1(av)
        } else if (is.finite(terminal_time)) {
          alive[i, tt] <- t < terminal_time
        } else {
          alive[i, tt] <- TRUE
        }

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
    entity_levels = entity_levels,
    entity_tags = entity_tags,
    schema = schema0,
    ctx = ctx,
    time_spec = engine$time_spec,
    seed = seed,
    S = S,
    P = P
  )

  if (return == "none") return(invisible(NULL))

  # Memory-light summaries (do not materialize a flux_forecast).
  


  x <- new_forecast(
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
  summary_stats <- match.arg(summary_stats, c("both", "event_prob", "state"))

  res <- list()

  if (summary_stats %in% c("event_prob", "both")) {
    if (is.null(summary_spec)) stop("summary_spec must be provided when summary_stats includes 'event_prob'.", call. = FALSE)
    res$event_prob <- do.call(event_prob, c(list(x = x), summary_spec))
  }

  if (summary_stats %in% c("state", "both")) {
    res$state <- state_summary(x, vars = vars, times = times)
  }

  if (summary_stats == "event_prob") return(res$event_prob)
  if (summary_stats == "state") return(res$state)
  return(res)
}

if (return == "object") return(x)

  x
}

`%||%` <- function(x, y) if (is.null(x)) y else x
