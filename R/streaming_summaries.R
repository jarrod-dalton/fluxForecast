# ------------------------------------------------------------------------------
# Streaming (memory-light) forecast summaries
#
# These helpers run S simulations per (entity, param_set) and return compact
# summary objects without materializing a full flux_forecast object.
#
# Default semantics:
# - Fixed cohort defined at start_time (default time0 = min(times)).
# - Base lifecycle eligibility uses schema `alive` when present; otherwise
#   bundle$terminal_events when declared; otherwise lifecycle is active where defined.
# - Optional event-free conditioning by start_time (terminal_events,
#   condition_on_events).
# - Optional eligible(snapshot, time, ctx) predicate evaluated at start_time.
# ------------------------------------------------------------------------------

.build_run_grid <- function(n_entities, n_param_sets, S) {
  # Returns a data.frame with one row per streaming execution unit.
  # NOTE: Do not name this column `run_id`; canonical run_id is owned by fluxCore::run_cohort().
  entity_id <- rep(seq_len(n_entities), each = n_param_sets * S)
  param_draw_id <- rep(rep(seq_len(n_param_sets), each = S), times = n_entities)
  sim_id <- rep.int(seq_len(S), times = n_entities * n_param_sets)
  data.frame(
    stream_id = seq_len(n_entities * n_param_sets * S),
    entity_id = entity_id,
    param_draw_id = param_draw_id,
    sim_id = sim_id,
    stringsAsFactors = FALSE
  )
}

.seed_for_stream <- function(seed, stream_id) {
  if (is.null(seed)) return(NULL)
  as.integer(seed + stream_id)
}

.eligibility_at_start <- function(
  entity,
  events_df,
  start_time,
  model_has_alive,
  model_terminal_events,
  terminal_events,
  condition_on_events,
  eligible,
  ctx
) {
  # Must be defined at start_time
  if (entity$last_time < start_time) return(FALSE)
  snap <- entity$snapshot_at_time(start_time)

  if (is_true1(model_has_alive)) {
    if (is.null(snap$alive)) {
      stop("snapshot_at_time() did not return an 'alive' field. Ensure schema includes 'alive' (logical).", call. = FALSE)
    }
    # Treat NA as not eligible (unknown lifecycle status at start_time).
    if (is.na(snap$alive) || !identical(snap$alive, TRUE)) return(FALSE)
  } else if (!is.null(model_terminal_events)) {
    ft_model <- .fluxf_first_event_time_any(events_df, model_terminal_events)
    if (is.finite(ft_model) && ft_model <= start_time) return(FALSE)
  }

  if (!is.null(terminal_events)) {
    ft <- .fluxf_first_event_time_any(events_df, terminal_events)
    if (is.finite(ft) && ft <= start_time) return(FALSE)
  }
  if (!is.null(condition_on_events)) {
    ft <- .fluxf_first_event_time_any(events_df, condition_on_events)
    if (is.finite(ft) && ft <= start_time) return(FALSE)
  }

  if (!is.null(eligible)) {
    if (!is.function(eligible)) stop("eligible must be a function if provided.", call. = FALSE)
    out <- eligible(snap, start_time, ctx)
    return(identical(out, TRUE))
  }

  TRUE
}

event_prob_forecast <- function(
  engine,
  entities,
  times,
  event,
  by = c("run", "entity", "entity_param_draw"),
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
  if (inherits(entities, "Entity")) entities <- list(p1 = entities)
  if (!is.list(entities) || length(entities) == 0L) stop("entities must be a non-empty list of Entity objects.", call. = FALSE)
  times <- sort(unique(.fluxf_as_numeric_time(times, name = "times", ctx = ctx, time_spec = engine$time_spec)))
  if (length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)

  if (is.null(start_time)) start_time <- min(times)
  start_time <- .fluxf_as_numeric_time(start_time, name = "start_time", ctx = ctx, time_spec = engine$time_spec)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be one of times (v1 restriction).", call. = FALSE)

  event <- unique(as.character(event))
  if (length(event) < 1L) stop("event must be a non-empty character vector.", call. = FALSE)
  if (!is.null(terminal_events)) terminal_events <- unique(as.character(terminal_events))
  if (!is.null(condition_on_events)) condition_on_events <- unique(as.character(condition_on_events))

  has_alive_by_entity <- vapply(entities, function(p) "alive" %in% names(p$schema), logical(1))
  if (any(has_alive_by_entity) && !all(has_alive_by_entity)) {
    stop("All entities must either define 'alive' in schema or all omit it.", call. = FALSE)
  }
  model_has_alive <- all(has_alive_by_entity)
  model_terminal_events <- .fluxf_bundle_terminal_events(engine$bundle)
  model_event_catalog <- .fluxf_bundle_event_catalog(engine$bundle)

  if (!model_has_alive) {
    if (!is.null(model_terminal_events)) {
      .fluxf_warn_once(
        "streaming_lifecycle_terminal_fallback",
        "Model schema omits 'alive'; deriving lifecycle status from bundle$terminal_events."
      )
    } else {
      .fluxf_warn_once(
        "streaming_lifecycle_defined_fallback",
        "Model schema omits 'alive' and bundle$terminal_events is not set; treating lifecycle status as active wherever runs are defined."
      )
    }
  }

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }

  N <- length(entities)
  P <- length(param_sets)

  entity_tags <- vapply(entities, function(p) {
    if (!is.null(p$id)) as.character(p$id) else NA_character_
  }, character(1))
  S <- as.integer(S)
  if (S < 1L) stop("S must be a positive integer.", call. = FALSE)
  horizon <- max(times)
  grid <- .build_run_grid(N, P, S)
  grid$entity_tag <- entity_tags[grid$entity_id]

  # per-run worker
  one_run <- function(row) {
    stream_id <- row$stream_id
    pid <- row$entity_id
    did <- row$param_draw_id
    ptag <- row$entity_tag

    if (!is.null(seed)) set.seed(.seed_for_stream(as.integer(seed), stream_id))

    p0 <- entities[[pid]]
    p <- p0$clone(deep = TRUE)

    ctx_base <- .fluxf_ctx_for_draw(ctx, param_draw_id = did, n_param_sets = P)
    ctx_run <- if (is.null(ctx_base)) list() else ctx_base
    ctx_run$params <- param_sets[[did]]

    out <- engine$run(
      entity = p,
      max_events = max_events,
      max_time = horizon,
      return_observations = FALSE,
      ctx = ctx_run
    )

    ev <- out$events

    eligible0 <- .eligibility_at_start(
      entity = out$entity,
      events_df = ev,
      start_time = start_time,
      model_has_alive = model_has_alive,
      model_terminal_events = model_terminal_events,
      terminal_events = terminal_events,
      condition_on_events = condition_on_events,
      eligible = eligible,
      ctx = ctx_run
    )

    if (!is_true1(eligible0)) {
      return(list(eligible = FALSE, contrib = integer(length(times)), entity_id = row$entity_id, entity_tag = ptag, param_draw_id = row$param_draw_id, sim_id = row$sim_id))
    }

    ft_event <- .fluxf_first_event_time_any(ev, event)
    contrib <- as.integer(ft_event <= times)
    list(
      eligible = TRUE,
      contrib = contrib,
      entity_id = row$entity_id,
      entity_tag = ptag,
      param_draw_id = row$param_draw_id,
      sim_id = row$sim_id,
      observed_event_types = sort(unique(as.character(ev$event_type)))
    )
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

  if (!model_has_alive && !is.null(model_terminal_events) && is.null(model_event_catalog)) {
    observed <- sort(unique(unlist(lapply(parts, function(z) z$observed_event_types))))
    unseen <- setdiff(model_terminal_events, observed)
    if (length(unseen) > 0L) {
      .fluxf_warn_once(
        "streaming_terminal_unseen_without_catalog",
        paste0(
          "bundle$terminal_events contains labels not observed in this streaming forecast run (and no bundle$event_catalog is declared): ",
          paste(unseen, collapse = ", ")
        )
      )
    }
  }

  n_eligible <- sum(vapply(parts, function(z) is_true1(z$eligible), logical(1)))

  group_cols <- switch(
    by,
    run = character(0),
    entity = "entity_id",
    entity_param_draw = c("entity_id", "param_draw_id")
  )

  if (n_eligible == 0L) {
    if (length(group_cols) == 0) {
      res <- data.frame(time = times, n_eligible = 0L, n_events = 0L, event_prob = NA_real_)
    } else {
      res <- data.frame(time = numeric(0), n_eligible = integer(0), n_events = integer(0), event_prob = numeric(0), stringsAsFactors = FALSE)
      for (gc in rev(group_cols)) res[[gc]] <- integer(0)
      res <- res[, c(group_cols, "time", "n_eligible", "n_events", "event_prob"), drop = FALSE]
    }
  } else if (length(group_cols) == 0) {
    counts <- Reduce(`+`, lapply(parts, `[[`, "contrib"))
    res <- data.frame(
      time = times,
      n_eligible = rep.int(n_eligible, length(times)),
      n_events = counts,
      event_prob = counts / n_eligible
    )
  } else {
    # aggregate by group
    part_df <- data.frame(
      entity_id = vapply(parts, function(z) z$entity_id, integer(1)),
      entity_tag = vapply(parts, function(z) z$entity_tag, character(1)),
      param_draw_id = vapply(parts, function(z) z$param_draw_id, integer(1)),
      eligible = vapply(parts, function(z) is_true1(z$eligible), logical(1)),
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
                   event_prob = counts / n_elig_k,
                   stringsAsFactors = FALSE)
      )
      rows_out[[ii]] <- dfk
    }
    rows_out <- rows_out[seq_len(ii)]
    res <- if (length(rows_out) == 0L) {
      tmp <- data.frame(time = numeric(0), n_eligible = integer(0), n_events = integer(0), event_prob = numeric(0), stringsAsFactors = FALSE)
      for (gc in rev(group_cols)) tmp[[gc]] <- integer(0)
      tmp <- tmp[, c(group_cols, setdiff(names(tmp), group_cols)), drop = FALSE]
      tmp
    } else {
      outk <- do.call(rbind, rows_out)
      rownames(outk) <- NULL
      outk
    }
  }
  res$risk <- res$event_prob

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
  new_event_prob(spec = spec, cohort = cohort, result = res)
}

state_summary_forecast <- function(
  engine,
  entities,
  times,
  vars,
  by = c("run", "entity", "entity_param_draw"),
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
  if (missing(vars) || is.null(vars) || length(vars) < 1L) stop("vars must be a non-empty character vector.", call. = FALSE)
  vars <- unique(as.character(vars))

  if (inherits(entities, "Entity")) entities <- list(p1 = entities)
  if (!is.list(entities) || length(entities) == 0L) stop("entities must be a non-empty list of Entity objects.", call. = FALSE)

  times <- sort(unique(.fluxf_as_numeric_time(times, name = "times", ctx = ctx, time_spec = engine$time_spec)))
  if (length(times) < 1L || any(!is.finite(times))) stop("times must be a non-empty numeric vector.", call. = FALSE)

  if (is.null(start_time)) start_time <- min(times)
  start_time <- .fluxf_as_numeric_time(start_time, name = "start_time", ctx = ctx, time_spec = engine$time_spec)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be one of times (v1 restriction).", call. = FALSE)

  if (is.null(param_sets)) {
    param_sets <- list(engine$bundle$params %||% list())
  } else {
    if (!is.list(param_sets) || length(param_sets) < 1L) stop("param_sets must be a non-empty list of parameter lists.", call. = FALSE)
  }

  has_alive_by_entity <- vapply(entities, function(p) "alive" %in% names(p$schema), logical(1))
  if (any(has_alive_by_entity) && !all(has_alive_by_entity)) {
    stop("All entities must either define 'alive' in schema or all omit it.", call. = FALSE)
  }
  model_has_alive <- all(has_alive_by_entity)
  model_terminal_events <- .fluxf_bundle_terminal_events(engine$bundle)
  model_event_catalog <- .fluxf_bundle_event_catalog(engine$bundle)

  if (!model_has_alive) {
    if (!is.null(model_terminal_events)) {
      .fluxf_warn_once(
        "streaming_state_lifecycle_terminal_fallback",
        "Model schema omits 'alive'; deriving lifecycle status from bundle$terminal_events."
      )
    } else {
      .fluxf_warn_once(
        "streaming_state_lifecycle_defined_fallback",
        "Model schema omits 'alive' and bundle$terminal_events is not set; treating lifecycle status as active wherever runs are defined."
      )
    }
  }

  N <- length(entities)
  P <- length(param_sets)

  entity_tags <- vapply(entities, function(p) {
    if (!is.null(p$id)) as.character(p$id) else NA_character_
  }, character(1))
  S <- as.integer(S)
  if (S < 1L) stop("S must be a positive integer.", call. = FALSE)
  horizon <- max(times)
  grid <- .build_run_grid(N, P, S)
  grid$entity_tag <- entity_tags[grid$entity_id]

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
    stream_id <- row$stream_id
    pid <- row$entity_id
    did <- row$param_draw_id
    ptag <- row$entity_tag

    if (!is.null(seed)) set.seed(.seed_for_stream(as.integer(seed), stream_id))

    p0 <- entities[[pid]]
    p <- p0$clone(deep = TRUE)

    ctx_base <- .fluxf_ctx_for_draw(ctx, param_draw_id = did, n_param_sets = P)
    ctx_run <- if (is.null(ctx_base)) list() else ctx_base
    ctx_run$params <- param_sets[[did]]

    out <- engine$run(
      entity = p,
      max_events = max_events,
      max_time = horizon,
      return_observations = FALSE,
      ctx = ctx_run
    )

    ev <- out$events

    eligible0 <- .eligibility_at_start(
      entity = out$entity,
      events_df = ev,
      start_time = start_time,
      model_has_alive = model_has_alive,
      model_terminal_events = model_terminal_events,
      terminal_events = terminal_events,
      condition_on_events = condition_on_events,
      eligible = eligible,
      ctx = ctx_run
    )

    if (!is_true1(eligible0)) return(NULL)

    # Eligible cohort contribution: accumulate within-run summaries.
    num_part <- blank_num_part()
    cat_part <- blank_cat_part()

    # Track whether each var ever takes a value outside {0,1} (or non-integer)
    # among eligible observations. This lets us treat true binary numerics as
    # categorical in the final output.
    bin_track <- stats::setNames(lapply(vars, function(v) list(min = Inf, max = -Inf, non_int = FALSE)), vars)

    p <- out$entity
    ft_model_terminal <- .fluxf_first_event_time_any(ev, model_terminal_events)
    for (tt in seq_along(times)) {
      t <- times[[tt]]
      if (p$last_time < t) next
      snap <- p$snapshot_at_time(t)
      if (is_true1(model_has_alive)) {
        if (is.na(snap$alive) || !identical(snap$alive, TRUE)) next
      } else if (is.finite(ft_model_terminal) && t >= ft_model_terminal) {
        next
      }
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
          if (!is_true1(all.equal(val, round(val)))) bt$non_int <- TRUE
          bin_track[[v]] <- bt

          # Also accumulate categorical counts for (0,1) values so we can
          # later decide whether this variable is truly binary.
          if (is_true1(all.equal(val, 0)) || is_true1(all.equal(val, 1))) {
            nm <- as.character(as.integer(val))
            slot <- cat_part[[v]][[tt]]
            if (is.null(slot)) slot <- integer(0)
            prev <- if (!is.null(names(slot)) && (nm %in% names(slot))) slot[[nm]] else 0L
            slot[[nm]] <- prev + 1L
            cat_part[[v]][[tt]] <- slot
          }
        } else {
          nm <- as.character(val)
          slot <- cat_part[[v]][[tt]]
          if (is.null(slot)) slot <- integer(0)
          prev <- if (!is.null(names(slot)) && (nm %in% names(slot))) slot[[nm]] else 0L
          slot[[nm]] <- prev + 1L
          cat_part[[v]][[tt]] <- slot
        }
      }
    }

    list(
      eligible = TRUE,
      entity_id = pid,
      entity_tag = ptag,
      param_draw_id = did,
      sim_id = row$sim_id,
      num = num_part,
      cat = cat_part,
      bin_track = bin_track,
      observed_event_types = sort(unique(as.character(ev$event_type)))
    )
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

  if (!model_has_alive && !is.null(model_terminal_events) && is.null(model_event_catalog)) {
    observed <- sort(unique(unlist(lapply(outs, function(z) z$observed_event_types))))
    unseen <- setdiff(model_terminal_events, observed)
    if (length(unseen) > 0L) {
      .fluxf_warn_once(
        "streaming_state_terminal_unseen_without_catalog",
        paste0(
          "bundle$terminal_events contains labels not observed in this streaming state summary run (and no bundle$event_catalog is declared): ",
          paste(unseen, collapse = ", ")
        )
      )
    }
  }

  # Group eligible run contributions
  outs <- Filter(Negate(is.null), outs)

  group_cols <- switch(
    by,
    run = character(0),
    entity = c("entity_id"),
    entity_param_draw = c("entity_id", "param_draw_id")
  )

  key_of <- function(o) {
    switch(
      by,
      run = "__all__",
      entity = as.character(o$entity_id),
      entity_param_draw = paste0(o$entity_id, "|", o$param_draw_id)
    )
  }

  outs_by_key <- split(outs, vapply(outs, key_of, character(1)))

  # Helper to materialize one group
  materialize_one_group <- function(outs_g, key) {
    n_eligible_total <- 0L
    num_tot <- blank_num_part()
    cat_tot <- blank_cat_part()
    names(num_tot) <- vars
    names(cat_tot) <- vars
    bin_tot <- stats::setNames(lapply(vars, function(v) list(min = Inf, max = -Inf, non_int = FALSE)), vars)

    for (o in outs_g) {
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
        bt$non_int <- is_true1(bt$non_int) || is_true1(obt$non_int)
        bin_tot[[v]] <- bt

        for (tt in seq_along(times)) {
          slot <- o$cat[[v]][[tt]]
          if (is.null(slot)) next
          cur <- cat_tot[[v]][[tt]]
          if (is.null(cur)) cur <- integer(0)
          for (nm in names(slot)) {
            prev <- if (!is.null(names(cur)) && (nm %in% names(cur))) cur[[nm]] else 0L
            cur[[nm]] <- prev + slot[[nm]]
          }
          cat_tot[[v]][[tt]] <- cur
        }
      }
    }

    # Materialize numeric
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
      numeric[[v]] <- data.frame(time = times, n = as.integer(n), mean = mean, sd = sd, min = mn, max = mx, stringsAsFactors = FALSE)
    }

    # Materialize categorical
    categorical <- list()
    for (v in vars) {
      rows_out <- list()
      for (tt in seq_along(times)) {
        slot <- cat_tot[[v]][[tt]]
        bt <- bin_tot[[v]]
        is_binary_numeric <- is.finite(bt$min) && is.finite(bt$max) && !is_true1(bt$non_int) && bt$min >= 0 && bt$max <= 1
        if (!is_binary_numeric && !is.null(slot) && all(names(slot) %in% c("0", "1"))) slot <- NULL
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
      categorical[[v]] <- if (length(rows_out) == 0L) data.frame(time = numeric(0), level = character(0), n = integer(0), p = numeric(0), stringsAsFactors = FALSE) else do.call(rbind, rows_out)
    }

    # Attach group columns
    if (by != "run") {
      if (by == "entity") {
        pid <- outs_g[[1]]$entity_id
        ptag <- outs_g[[1]]$entity_tag
        ptag <- outs_g[[1]]$entity_tag
        for (v in vars) {
          numeric[[v]]$entity_id <- pid
          numeric[[v]]$entity_tag <- ptag
          numeric[[v]] <- numeric[[v]][, c("entity_id", "entity_tag", setdiff(names(numeric[[v]]), c("entity_id","entity_tag"))), drop = FALSE]
          categorical[[v]]$entity_id <- pid
          categorical[[v]]$entity_tag <- ptag
          categorical[[v]] <- categorical[[v]][, c("entity_id", "entity_tag", setdiff(names(categorical[[v]]), c("entity_id","entity_tag"))), drop = FALSE]
        }
      } else if (by == "entity_param_draw") {
        pid <- outs_g[[1]]$entity_id
        did <- outs_g[[1]]$param_draw_id
        for (v in vars) {
          numeric[[v]]$entity_id <- pid
          numeric[[v]]$entity_tag <- ptag
          numeric[[v]]$param_draw_id <- did
          numeric[[v]] <- numeric[[v]][, c("entity_id", "entity_tag", "param_draw_id", setdiff(names(numeric[[v]]), c("entity_id","entity_tag","param_draw_id"))), drop = FALSE]
          categorical[[v]]$entity_id <- pid
          categorical[[v]]$entity_tag <- ptag
          categorical[[v]]$param_draw_id <- did
          categorical[[v]] <- categorical[[v]][, c("entity_id", "entity_tag", "param_draw_id", setdiff(names(categorical[[v]]), c("entity_id","entity_tag","param_draw_id"))), drop = FALSE]
        }
      }
    }

    list(meta = list(n_eligible = n_eligible_total), numeric = numeric, categorical = categorical)
  }

  per_group <- lapply(names(outs_by_key), function(k) materialize_one_group(outs_by_key[[k]], k))
  names(per_group) <- names(outs_by_key)

  # Bind groups
  numeric <- stats::setNames(lapply(vars, function(v) do.call(rbind, lapply(per_group, function(g) g$numeric[[v]]))), vars)
  categorical <- stats::setNames(lapply(vars, function(v) do.call(rbind, lapply(per_group, function(g) g$categorical[[v]]))), vars)

  n_eligible_total <- sum(vapply(per_group, function(g) g$meta$n_eligible, integer(1)))

  list(
    meta = list(times = times, start_time = start_time, n_eligible = n_eligible_total, by = by),
    numeric = numeric,
    categorical = categorical
  )
}
