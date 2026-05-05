test_that("state_summary quantiles pool correctly across multiple parameter sets", {
  skip_if_not_installed("fluxCore")

  times <- c(0, 1, 2)

  # Extend the default schema with a numeric state variable we will track.
  schema <- test_entity_schema()
  schema[["x"]] <- list(
    type = "continuous",
    default = 0,
    coerce = as.numeric,
    validate = function(v) length(v) == 1L && is.finite(v)
  )
  schema[["age"]] <- list(type = "continuous", default = NA_real_, coerce = as.numeric, validate = function(v) length(v) == 1L && (is.na(v) || is.finite(v)))
  schema[["miles_to_work"]] <- list(type = "continuous", default = NA_real_, coerce = as.numeric, validate = function(v) length(v) == 1L && (is.na(v) || is.finite(v)))


  # Deterministic tick process:
  # - At each tick time t in param_ctx$params$times, set x <- shift + t.
  # - alive remains TRUE throughout.
  toy_bundle <- list(
    time_spec = fluxCore::time_spec(unit = "days"),
    propose_events = function(entity, param_ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      pid <- "tick"
      if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
      tt <- sort(as.numeric(param_ctx$params$times))

      # We want a tick at time 0 once, then strictly increasing thereafter.
      if (entity$last_j == 0L) {
        t_next <- tt[[1]]
      } else {
        cand <- tt[tt > entity$last_time]
        if (length(cand) == 0L) return(list())
        t_next <- cand[[1]]
      }

      list(tick = list(time_next = t_next, event_type = "TICK", process_id = pid))
    },
    transition = function(entity, event, param_ctx = NULL) {
      if (!identical(event$event_type, "TICK")) return(NULL)
      list(
        x = as.numeric(param_ctx$params$shift) + as.numeric(event$time_next),
        alive = TRUE,
        active_followup = TRUE
      )
    },
    stop = function(entity, event) {
      FALSE
    }
  )

  # One entity.
  pat <- fluxCore::Entity$new(
    init = list(age = 40, miles_to_work = 10, alive = TRUE, active_followup = TRUE, x = 0),
    schema = schema
  )

  # Five parameter sets, one run each. Using 5 ensures type=7 quantiles land exactly on order stats
  # for probs {0.25, 0.5, 0.75}.
  # Each param_set carries both the shift value AND the evaluation times.
  param_sets <- lapply(0:4, function(s) list(shift = s, times = times))

  engine <- fluxCore::Engine$new(bundle = toy_bundle)

  x <- forecast(
    engine = engine,
    entities = pat,
    times = times,
    vars = c("x", "alive", "active_followup"),
    S = 1,
    param_sets = param_sets,
    backend = "none",
    return = "object",
    seed = 123
  )

  ss <- state_summary(x, vars = "x", times = times)
  df <- ss[["x"]]

  # Expected pooled draws at each time:
  # time 0: 0..4, time 1: 1..5, time 2: 2..6
  exp_by_time <- list(
    `0` = 0:4,
    `1` = 1:5,
    `2` = 2:6
  )

  for (t in times) {
    row <- df[df$time == t, , drop = FALSE]
    expect_equal(row$n, 5L)
    vals <- exp_by_time[[as.character(t)]]
    expect_equal(row$min, min(vals))
    expect_equal(row$q1, as.numeric(stats::quantile(vals, 0.25, type = 7, names = FALSE)))
    expect_equal(row$median, as.numeric(stats::quantile(vals, 0.5, type = 7, names = FALSE)))
    expect_equal(row$q3, as.numeric(stats::quantile(vals, 0.75, type = 7, names = FALSE)))
    expect_equal(row$max, max(vals))
  }
})
