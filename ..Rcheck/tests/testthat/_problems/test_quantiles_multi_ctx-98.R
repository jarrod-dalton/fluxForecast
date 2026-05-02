# Extracted from test_quantiles_multi_ctx.R:98

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "fluxForecast", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("fluxCore")
times <- c(0, 1, 2)
schema <- test_entity_schema()
schema[["x"]] <- list(
    type = "continuous",
    default = 0,
    coerce = as.numeric,
    validate = function(v) length(v) == 1L && is.finite(v)
  )
schema[["age"]] <- list(type = "continuous", default = NA_real_, coerce = as.numeric, validate = function(v) length(v) == 1L && (is.na(v) || is.finite(v)))
schema[["miles_to_work"]] <- list(type = "continuous", default = NA_real_, coerce = as.numeric, validate = function(v) length(v) == 1L && (is.na(v) || is.finite(v)))
toy_bundle <- list(
    time_spec = fluxCore::time_spec(unit = "days"),
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      pid <- "tick"
      if (!is.null(process_ids) && !(pid %in% process_ids)) return(list())
      if (is.null(ctx) || is.null(ctx$times)) stop("ctx$times is required for this test")
      tt <- sort(as.numeric(ctx$times))

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
    transition = function(entity, event, ctx = NULL) {
      if (is.null(ctx) || is.null(ctx$params) || is.null(ctx$params$shift)) stop("ctx$params$shift required")
      if (!identical(event$event_type, "TICK")) return(NULL)
      list(
        x = as.numeric(ctx$params$shift) + as.numeric(event$time_next),
        alive = TRUE,
        active_followup = TRUE
      )
    },
    stop = function(entity, event, ctx = NULL) {
      FALSE
    }
  )
pat <- fluxCore::Entity$new(
    init = list(age = 40, miles_to_work = 10, alive = TRUE, active_followup = TRUE, x = 0),
    schema = schema
  )
ctx_list <- lapply(0:4, function(s) list(times = times, params = list(shift = s)))
provider <- list(load = function(model_spec, ...) toy_bundle)
engine <- fluxCore::Engine$new(provider = provider)
x <- forecast(
    engine = engine,
    entities = pat,
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
