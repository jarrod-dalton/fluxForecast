# ------------------------------------------------------------------------------
# Streaming (memory-light) forecast summaries
#
# These helpers run S simulations per (patient, param_set) and return compact
# summary objects without materializing a full ps_forecast object.
#
# v1 semantics:
# - Fixed cohort defined at start_time (default time0 = min(times)).
# - Base eligibility requires snapshot_at_time(start_time)$alive == TRUE.
# - Optional event-free conditioning by start_time (terminal_events,
#   condition_on_events).
# - Optional eligible(snapshot, time, ctx) predicate evaluated at start_time.
# ------------------------------------------------------------------------------

.build_run_grid <- function(n_patients, n_param_sets, S) {
  # Returns a data.frame with one row per run.
  patient_id <- rep(seq_len(n_patients), each = n_param_sets * S)
  param_set_id <- rep(rep(seq_len(n_param_sets), each = S), times = n_patients)
  sim_id <- rep.int(seq_len(S), times = n_patients * n_param_sets)
  data.frame(
    run_id = seq_len(n_patients * n_param_sets * S),
    patient_id = patient_id,
    param_set_id = param_set_id,
    sim_id = sim_id,
    stringsAsFactors = FALSE
  )
}

.seed_for_run <- function(seed, run_id) {
  if (is.null(seed)) return(NULL)
  as.integer(seed + run_id)
}

.first_event_time_any <- function(events_df, event_set) {
  if (is.null(events_df) || nrow(events_df) == 0L) return(Inf)
  et <- as.character(events_df$event_type)
  keep <- et %in% event_set
  if (!any(keep)) return(Inf)
  min(as.numeric(events_df$time[keep]))
}

.eligibility_at_start <- function(
  patient,
  events_df,
  start_time,
  terminal_events,
  condition_on_events,
  eligible,
  ctx
) {
  # Must be defined at start_time
  if (patient$last_time < start_time) return(FALSE)
  snap <- patient$snapshot_at_time(start_time)
  if (is.null(snap$alive)) {
    stop("snapshot_at_time() did not return an 'alive' field. Ensure schema includes 'alive' (logical).", call. = FALSE)
  }
  # Treat NA as not eligible (unknown vital status at start_time).
  if (is.na(snap$alive) || !identical(snap$alive, TRUE)) return(FALSE)

  if (!is.null(terminal_events)) {
    ft <- .first_event_time_any(events_df, terminal_events)
    if (is.finite(ft) && ft <= start_time) return(FALSE)
  }
  if (!is.null(condition_on_events)) {
    ft <- .first_event_time_any(events_df, condition_on_events)
    if (is.finite(ft) && ft <= start_time) return(FALSE)
  }

  if (!is.null(eligible)) {
    if (!is.function(eligible)) stop("eligible must be a function if provided.", call. = FALSE)
    out <- eligible(snap, start_time, ctx)
    return(identical(out, TRUE))
  }

  TRUE
}

#' Streaming risk summary without materializing a full ps_forecast
#'
#' @param engine A patientSimCore Engine.
#' @param patients A list of Patient objects (or a single Patient).
#' @param times Numeric vector of forecast times.
#' @param event Character vector of event types of interest.
#' @param S Simulations per (patient, param_set).
#' @param param_sets Optional list of parameter lists.
#' @param start_time Scalar time in `times` to define the eligible cohort (default min(times)).
#' @param terminal_events Optional event types that must be absent by start_time.
#' @param condition_on_events Optional event types that must be absent by start_time.
#' @param eligible Optional function(snapshot, time, ctx) -> TRUE/FALSE, evaluated at start_time.
#' @param ctx Optional list passed to eligible().
#' @param max_events Passed to Engine$run().
#' @param seed Optional base seed.
#' @param backend Parallel backend. One of c('none','mclapply','future').
#'   Use 'future' with future.apply (recommended for clusters/cloud).
#' @param n_workers Optional number of workers. Used for 'mclapply'. If backend='future'
#'   and n_workers is provided, risk_forecast() will temporarily set a local
#'   future plan (multisession) with that number of workers and then restore
#'   the previous plan.
#'
#' @return A ps_risk object.
#' @export
risk_forecast <- function(
  engine,
  patients,
  times,
  event,
  by = c("run", "patient", "patient_draw"),
  S = 200,
  param_sets = NULL,
  start_time = NULL,
  terminal_events = NULL,
  condition_on_events = NULL,
  eligible = NULL,
  ctx = NULL,
  max_events = 1000,
  seed = NULL,
  backend = c("none", "mclapply", "future"),
  n_workers = NULL
) {
  backend <- match.arg(backend)
  by <- match.arg(by)
  if (inherits(patients, "Patient")) patients <- list(p1 = patients)
  if (!is.list(patients) || length(patients) == 0L) stop("patients must be a non-empty list of Patient objects.", call. = FALSE)
  times <- sort(unique(as.numeric(times)))
  if (length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)

  if (is.null(start_time)) start_time <- min(times)
  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be one of times (v1 restriction).", call. = FALSE)

  event <- unique(as.character(event))
  if (length(event) < 1L) stop("event must be a non-empty character vector.", call. = FALSE)
  if (!is.null(terminal_events)) terminal_events <- unique(as.character(terminal_events))
  if (!is.null(condition_on_events)) condition_on_events <- unique(as.character(condition_on_events))

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }

  if (!is.null(ctx) && !is.list(ctx)) stop("ctx must be a list or NULL.", call. = FALSE)

  N <- length(patients)
  P <- length(param_sets)
  S <- as.integer(S)
  if (S < 1L) stop("S must be a positive integer.", call. = FALSE)
  horizon <- max(times)
  grid <- .build_run_grid(N, P, S)

  # per-run worker
  one_run <- function(row) {
    run_id <- row$run_id
    pid <- row$patient_id
    did <- row$param_set_id

    if (!is.null(seed)) set.seed(.seed_for_run(as.integer(seed), run_id))

    p0 <- patients[[pid]]
    p <- p0$clone(deep = TRUE)

    ctx_run <- if (is.null(ctx)) list() else ctx
    ctx_run$params <- param_sets[[did]]

    out <- engine$run(
      patient = p,
      max_events = max_events,
      max_time = horizon,
      return_observations = FALSE,
      ctx = ctx_run
    )

    ev <- out$events

    eligible0 <- .eligibility_at_start(
      patient = out$patient,
      events_df = ev,
      start_time = start_time,
      terminal_events = terminal_events,
      condition_on_events = condition_on_events,
      eligible = eligible,
      ctx = ctx_run
    )

    if (!ps_is_true1(eligible0)) {
      return(list(eligible = FALSE, contrib = integer(length(times)), patient_id = row$patient_id, draw_id = row$param_set_id, sim_id = row$sim_id))
    }

    ft_event <- .first_event_time_any(ev, event)
    contrib <- as.integer(ft_event <= times)
    list(eligible = TRUE, contrib = contrib, patient_id = row$patient_id, draw_id = row$param_set_id, sim_id = row$sim_id)
  }

  rows <- split(grid, seq_len(nrow(grid)))
  run_fun <- function(i) one_run(rows[[i]])

  parts <- switch(
    backend,
    none = lapply(seq_along(rows), run_fun),
    mclapply = {
      if (.Platform$OS.type == "windows") stop("backend='mclapply' is not supported on Windows.", call. = FALSE)
      if (!requireNamespace("parallel", quietly = TRUE)) stop("backend='mclapply' requires base R 'parallel'.", call. = FALSE)
      ncores <- if (is.null(n_workers)) max(1L, parallel::detectCores() - 1L) else as.integer(n_workers)
      if (!is.finite(ncores) || ncores < 1L) stop("n_workers must be a positive integer.", call. = FALSE)
      parallel::mclapply(seq_along(rows), run_fun, mc.cores = ncores)
    },
    future = {
      if (!requireNamespace("future.apply", quietly = TRUE)) stop("backend='future' requires the 'future.apply' package.", call. = FALSE)
      if (!requireNamespace("future", quietly = TRUE)) stop("backend='future' requires the 'future' package.", call. = FALSE)
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      if (!is.null(n_workers)) {
        n_workers <- as.integer(n_workers)
        if (!is.finite(n_workers) || n_workers < 1L) stop("n_workers must be a positive integer.", call. = FALSE)
        future::plan(future::multisession, workers = n_workers)
      }
      future.apply::future_lapply(seq_along(rows), run_fun)
    }
  )

  n_eligible <- sum(vapply(parts, function(z) ps_is_true1(z$eligible), logical(1)))

  group_cols <- switch(
    by,
    run = character(0),
    patient = "patient_id",
    patient_draw = c("patient_id", "draw_id")
  )

  if (n_eligible == 0L) {
    if (length(group_cols) == 0) {
      res <- data.frame(time = times, n_eligible = 0L, n_events = 0L, risk = NA_real_)
    } else {
      res <- data.frame(time = numeric(0), n_eligible = integer(0), n_events = integer(0), risk = numeric(0), stringsAsFactors = FALSE)
      for (gc in rev(group_cols)) res[[gc]] <- integer(0)
      res <- res[, c(group_cols, "time", "n_eligible", "n_events", "risk"), drop = FALSE]
    }
  } else if (length(group_cols) == 0) {
    counts <- Reduce(`+`, lapply(parts, `[[`, "contrib"))
    res <- data.frame(
      time = times,
      n_eligible = rep.int(n_eligible, length(times)),
      n_events = counts,
      risk = counts / n_eligible
    )
  } else {
    # aggregate by group
    part_df <- data.frame(
      patient_id = vapply(parts, function(z) z$patient_id, integer(1)),
      draw_id = vapply(parts, function(z) z$draw_id, integer(1)),
      eligible = vapply(parts, function(z) ps_is_true1(z$eligible), logical(1)),
      stringsAsFactors = FALSE
    )
    part_df$key <- interaction(part_df[, group_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
    idx_by <- split(seq_len(nrow(part_df)), part_df$key)

    rows_out <- vector("list", length(idx_by))
    ii <- 0L
    for (k in names(idx_by)) {
      ids <- idx_by[[k]]
      elig_ids <- ids[part_df$eligible[ids]]
      n_elig_k <- length(elig_ids)
      if (n_elig_k == 0L) next
      counts <- Reduce(`+`, lapply(parts[elig_ids], `[[`, "contrib"))
      ii <- ii + 1L
      # representative group values
      grp_vals <- part_df[ids[1], group_cols, drop = FALSE]
      dfk <- cbind(
        grp_vals,
        data.frame(time = times,
                   n_eligible = rep.int(n_elig_k, length(times)),
                   n_events = counts,
                   risk = counts / n_elig_k,
                   stringsAsFactors = FALSE)
      )
      rows_out[[ii]] <- dfk
    }
    rows_out <- rows_out[seq_len(ii)]
    res <- if (length(rows_out) == 0L) {
      tmp <- data.frame(time = numeric(0), n_eligible = integer(0), n_events = integer(0), risk = numeric(0), stringsAsFactors = FALSE)
      for (gc in rev(group_cols)) tmp[[gc]] <- integer(0)
      tmp <- tmp[, c(group_cols, setdiff(names(tmp), group_cols)), drop = FALSE]
      tmp
    } else {
      outk <- do.call(rbind, rows_out)
      rownames(outk) <- NULL
      outk
    }
  }

spec <- list(
    event = event,
    by = by,
    times = times,
    start_time = start_time,
    terminal_events = terminal_events,
    condition_on_events = condition_on_events,
    eligible_provided = !is.null(eligible),
    denom = "fixed"
  )
  cohort <- list(eligible_run_ids = integer(0), n_eligible = n_eligible)
  new_ps_risk(spec = spec, cohort = cohort, result = res)
}

#' Streaming state summaries without materializing a full ps_forecast
#'
#' Computes conditional summaries of state variables over a time grid among a
#' fixed eligible cohort defined at start_time.
#'
#' @param vars Character vector of state/derived variable names to summarize.
#' @param start_time Cohort definition time (default min(times)).
#' @param terminal_events,condition_on_events,eligible,ctx See risk_forecast().
#'
#' @return A list with elements:
#'   - meta: list
#'   - numeric: named list of data.frames (one per numeric var)
#'   - categorical: named list of data.frames (one per categorical var)
#' @export
state_summary_forecast <- function(
  engine,
  patients,
  times,
  vars,
  S = 200,
  param_sets = NULL,
  start_time = NULL,
  terminal_events = NULL,
  condition_on_events = NULL,
  eligible = NULL,
  ctx = NULL,
  max_events = 1000,
  seed = NULL,
  backend = c("none", "mclapply", "future"),
  n_workers = NULL
) {
  backend <- match.arg(backend)
  if (missing(vars) || is.null(vars) || length(vars) < 1L) stop("vars must be a non-empty character vector.", call. = FALSE)
  vars <- unique(as.character(vars))

  if (inherits(patients, "Patient")) patients <- list(p1 = patients)
  if (!is.list(patients) || length(patients) == 0L) stop("patients must be a non-empty list of Patient objects.", call. = FALSE)

  times <- sort(unique(as.numeric(times)))
  if (length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)

  if (is.null(start_time)) start_time <- min(times)
  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be one of times (v1 restriction).", call. = FALSE)

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }

  if (!is.null(ctx) && !is.list(ctx)) stop("ctx must be a list or NULL.", call. = FALSE)

  N <- length(patients)
  P <- length(param_sets)
  S <- as.integer(S)
  if (S < 1L) stop("S must be a positive integer.", call. = FALSE)
  horizon <- max(times)
  grid <- .build_run_grid(N, P, S)

  T <- length(times)

  # Each run returns compact partial sums; we reduce across runs.
  blank_num_part <- function() {
    out <- lapply(vars, function(v) list(n = integer(T), sum = numeric(T), sumsq = numeric(T), min = rep(Inf, T), max = rep(-Inf, T)))
    names(out) <- vars
    out
  }

  blank_cat_part <- function() {
    stats::setNames(lapply(vars, function(v) vector("list", T)), vars)
  }

  one_run <- function(row) {
    run_id <- row$run_id
    pid <- row$patient_id
    did <- row$param_set_id

    if (!is.null(seed)) set.seed(.seed_for_run(as.integer(seed), run_id))

    p0 <- patients[[pid]]
    p <- p0$clone(deep = TRUE)

    ctx_run <- if (is.null(ctx)) list() else ctx
    ctx_run$params <- param_sets[[did]]

    out <- engine$run(
      patient = p,
      max_events = max_events,
      max_time = horizon,
      return_observations = FALSE,
      ctx = ctx_run
    )

    ev <- out$events

    eligible0 <- .eligibility_at_start(
      patient = out$patient,
      events_df = ev,
      start_time = start_time,
      terminal_events = terminal_events,
      condition_on_events = condition_on_events,
      eligible = eligible,
      ctx = ctx_run
    )

    if (!ps_is_true1(eligible0)) return(NULL)

    # Eligible cohort contribution: accumulate within-run summaries.
    num_part <- blank_num_part()
    cat_part <- blank_cat_part()

    # Track whether each var ever takes a value outside {0,1} (or non-integer)
    # among eligible observations. This lets us treat true binary numerics as
    # categorical in the final output.
    bin_track <- stats::setNames(lapply(vars, function(v) list(min = Inf, max = -Inf, non_int = FALSE)), vars)

    p <- out$patient
    for (tt in seq_along(times)) {
      t <- times[[tt]]
      if (p$last_time < t) next
      snap <- p$snapshot_at_time(t)
      if (is.na(snap$alive) || !identical(snap$alive, TRUE)) next
      for (v in vars) {
        val <- snap[[v]]
        if (is.null(val)) next
        if (is.numeric(val) && length(val) == 1L && is.finite(val)) {
          # Always accumulate numeric moments (used for continuous vars).
          num_part[[v]]$n[[tt]] <- num_part[[v]]$n[[tt]] + 1L
          num_part[[v]]$sum[[tt]] <- num_part[[v]]$sum[[tt]] + val
          num_part[[v]]$sumsq[[tt]] <- num_part[[v]]$sumsq[[tt]] + (val * val)
          num_part[[v]]$min[[tt]] <- min(num_part[[v]]$min[[tt]], val)
          num_part[[v]]$max[[tt]] <- max(num_part[[v]]$max[[tt]], val)
          bt <- bin_track[[v]]
          bt$min <- min(bt$min, val)
          bt$max <- max(bt$max, val)
          if (!ps_is_true1(all.equal(val, round(val)))) bt$non_int <- TRUE
          bin_track[[v]] <- bt

          # Also accumulate categorical counts for (0,1) values so we can
          # later decide whether this variable is truly binary.
          if (ps_is_true1(all.equal(val, 0)) || ps_is_true1(all.equal(val, 1))) {
            nm <- as.character(as.integer(val))
            slot <- cat_part[[v]][[tt]]
            if (is.null(slot)) slot <- integer(0)
            slot[[nm]] <- (slot[[nm]] %||% 0L) + 1L
            cat_part[[v]][[tt]] <- slot
          }
        } else {
          nm <- as.character(val)
          slot <- cat_part[[v]][[tt]]
          if (is.null(slot)) slot <- integer(0)
          slot[[nm]] <- (slot[[nm]] %||% 0L) + 1L
          cat_part[[v]][[tt]] <- slot
        }
      }
    }

    list(num = num_part, cat = cat_part, bin_track = bin_track)
  }

  rows <- split(grid, seq_len(nrow(grid)))
  run_fun <- function(i) one_run(rows[[i]])

  outs <- switch(
    backend,
    none = lapply(seq_along(rows), run_fun),
    mclapply = {
      if (.Platform$OS.type == "windows") stop("backend='mclapply' is not supported on Windows.", call. = FALSE)
      if (!requireNamespace("parallel", quietly = TRUE)) stop("backend='mclapply' requires base R 'parallel'.", call. = FALSE)
      ncores <- if (is.null(n_workers)) max(1L, parallel::detectCores() - 1L) else as.integer(n_workers)
      if (!is.finite(ncores) || ncores < 1L) stop("n_workers must be a positive integer.", call. = FALSE)
      parallel::mclapply(seq_along(rows), run_fun, mc.cores = ncores)
    },
    future = {
      if (!requireNamespace("future.apply", quietly = TRUE)) stop("backend='future' requires the 'future.apply' package.", call. = FALSE)
      if (!requireNamespace("future", quietly = TRUE)) stop("backend='future' requires the 'future' package.", call. = FALSE)
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      if (!is.null(n_workers)) {
        n_workers <- as.integer(n_workers)
        if (!is.finite(n_workers) || n_workers < 1L) stop("n_workers must be a positive integer.", call. = FALSE)
        future::plan(future::multisession, workers = n_workers)
      }
      future.apply::future_lapply(seq_along(rows), run_fun)
    }
  )

  n_eligible_total <- 0L
  num_tot <- blank_num_part()
  cat_tot <- blank_cat_part()
  names(num_tot) <- vars
  names(cat_tot) <- vars

  bin_tot <- stats::setNames(lapply(vars, function(v) list(min = Inf, max = -Inf, non_int = FALSE)), vars)

  for (o in outs) {
    if (is.null(o)) next
    n_eligible_total <- n_eligible_total + 1L
    for (v in vars) {
      num_tot[[v]]$n <- num_tot[[v]]$n + o$num[[v]]$n
      num_tot[[v]]$sum <- num_tot[[v]]$sum + o$num[[v]]$sum
      num_tot[[v]]$sumsq <- num_tot[[v]]$sumsq + o$num[[v]]$sumsq
      num_tot[[v]]$min <- pmin(num_tot[[v]]$min, o$num[[v]]$min)
      num_tot[[v]]$max <- pmax(num_tot[[v]]$max, o$num[[v]]$max)
      bt <- bin_tot[[v]]
      obt <- o$bin_track[[v]]
      bt$min <- min(bt$min, obt$min)
      bt$max <- max(bt$max, obt$max)
      bt$non_int <- ps_is_true1(bt$non_int) || ps_is_true1(obt$non_int)
      bin_tot[[v]] <- bt

      for (tt in seq_along(times)) {
        slot <- o$cat[[v]][[tt]]
        if (is.null(slot)) next
        cur <- cat_tot[[v]][[tt]]
        if (is.null(cur)) cur <- integer(0)
        for (nm in names(slot)) {
          cur[[nm]] <- (cur[[nm]] %||% 0L) + slot[[nm]]
        }
        cat_tot[[v]][[tt]] <- cur
      }
    }
  }

  # Materialize outputs
  numeric <- list()
  for (v in vars) {
    n <- num_tot[[v]]$n
    sum <- num_tot[[v]]$sum
    sumsq <- num_tot[[v]]$sumsq
    mean <- ifelse(n > 0L, sum / n, NA_real_)
    var <- ifelse(n > 1L, (sumsq - (sum * sum) / n) / (n - 1L), NA_real_)
    sd <- sqrt(var)
    mn <- ifelse(is.finite(num_tot[[v]]$min), num_tot[[v]]$min, NA_real_)
    mx <- ifelse(is.finite(num_tot[[v]]$max), num_tot[[v]]$max, NA_real_)
    numeric[[v]] <- data.frame(time = times, n = as.integer(n), mean = mean, sd = sd, min = mn, max = mx)
  }

  categorical <- list()
  for (v in vars) {
    rows_out <- list()
    for (tt in seq_along(times)) {
      slot <- cat_tot[[v]][[tt]]
      # If this var is not truly binary, drop accidental (0,1) counts collected from numeric values.
      bt <- bin_tot[[v]]
      is_binary_numeric <- is.finite(bt$min) && is.finite(bt$max) && !ps_is_true1(bt$non_int) && bt$min >= 0 && bt$max <= 1
      if (!is_binary_numeric && !is.null(slot) && all(names(slot) %in% c('0','1'))) slot <- NULL
      if (is.null(slot) || length(slot) == 0L) next
      nn <- sum(slot)
      rows_out[[tt]] <- data.frame(
        time = rep.int(times[[tt]], length(slot)),
        level = names(slot),
        n = as.integer(slot),
        p = as.numeric(slot) / nn,
        stringsAsFactors = FALSE
      )
    }
    categorical[[v]] <- if (length(rows_out) == 0L) data.frame(time = numeric(0), level = character(0), n = integer(0), p = numeric(0)) else do.call(rbind, rows_out)
  }

  list(
    meta = list(times = times, start_time = start_time, n_eligible = n_eligible_total),
    numeric = numeric,
    categorical = categorical
  )
}
