test_that("state_summary quantiles pool correctly across multiple ctx (parameter sets)", {
  skip_if_not_installed("patientSimCore")

  times <- c(0, 1, 2)

  # Extend the default schema with a numeric state variable we will track.
  schema <- patientSimCore::default_patient_schema()
  schema[["x"]] <- list(
    default = 0,
    coerce = as.numeric,
    validate = function(v) length(v) == 1L && is.finite(v)
  )

  # Deterministic tick process:
  # - At each tick time t in ctx$times, set x <- shift + t.
  # - alive remains TRUE throughout.
  toy_bundle <- list(
    propose_events = function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      pid <- "tick"
      if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
      if (is.null(ctx) || is.null(ctx$times)) stop("ctx$times is required for this test")
      tt <- sort(as.numeric(ctx$times))

      # We want a tick at time 0 once, then strictly increasing thereafter.
      if (patient$last_j == 0L) {
        t_next <- tt[[1]]
      } else {
        cand <- tt[tt > patient$last_time]
        if (length(cand) == 0L) return(list())
        t_next <- cand[[1]]
      }

      list(tick = list(time_next = t_next, event_type = "TICK", process_id = pid))
    },
    transition = function(patient, event, ctx = NULL) {
      if (is.null(ctx) || is.null(ctx$params) || is.null(ctx$params$shift)) stop("ctx$params$shift required")
      if (!identical(event$event_type, "TICK")) return(NULL)
      list(
        x = as.numeric(ctx$params$shift) + as.numeric(event$time_next),
        alive = TRUE,
        active_followup = TRUE
      )
    },
    stop = function(patient, event, ctx = NULL) {
      FALSE
    }
  )

  # One patient.
  pat <- patientSimCore::new_patient(
    init = list(age = 40, miles_to_work = 10, alive = TRUE, active_followup = TRUE, x = 0),
    schema = schema
  )

  # Five parameter sets, one run each. Using 5 ensures type=7 quantiles land exactly on order stats
  # for probs {0.25, 0.5, 0.75}.
  ctx_list <- lapply(0:4, function(s) list(times = times, params = list(shift = s)))

  provider <- list(load = function(model_spec, ...) toy_bundle)
  engine <- patientSimCore::Engine$new(provider = provider)

  x <- forecast(
    engine = engine,
    patients = pat,
    times = times,
    vars = c("x", "alive", "active_followup"),
    S = 1,
        # Five parameter sets, one draw each. We keep param_sets empty here and encode
    # the deterministic shift inside ctx so we can test pooling across ctx.
    param_sets = replicate(5, list(), simplify = FALSE),
    ctx = ctx_list,
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
