# Extracted from test_state_summary_by.R:68

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "fluxForecast", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(testthat)
library(fluxCore)
library(fluxForecast)
make_toy_bundle <- function() {
  propose_events <- function(entity, ctx, ...) {
    props <- list()
    if (entity$last_time < 2) {
      props$visit <- list(time_next = entity$last_time + 1, event_type = "visit")
    }
    props
  }

  transition <- function(entity, event, ctx, ...) {
    if (event$event_type == "visit") {
      # no-op
      NULL
    } else {
      NULL
    }
  }

  stop <- function(entity, event, ctx, ...) {
    entity$last_time >= 2
  }

  list(
    time_spec = fluxCore::time_spec(unit = "days"),
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    refresh_rules = function(...) "ALL"
  )
}

# test -------------------------------------------------------------------------
schema <- test_entity_schema()
schema[["phase"]] <- list(type = "categorical", levels = c("waitlist","post_mi"), default = "waitlist", coerce = as.character)
schema[["x"]] <- list(type = "continuous", default = 0, coerce = as.numeric, validate = function(v) length(v) == 1L && is.finite(v))
p1 <- fluxCore::Entity$new(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)
p2 <- fluxCore::Entity$new(init = list(alive = TRUE, phase = "waitlist", x = 10), schema = schema, time0 = 0)
bundle <- make_toy_bundle()
provider <- list(load = function(model_spec, ...) bundle)
engine <- Engine$new(provider = provider)
fx <- forecast(
    engine = engine,
    entities = list(p1 = p1, p2 = p2),
    times = c(0, 1, 2),
    S = 2,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object"
  )
ss <- state_summary(fx, vars = "x", times = c(0), by = "entity")
expect_true(is.list(ss))
expect_true("x" %in% names(ss))
df <- ss[["x"]]
expect_true(all(c("entity_id", "time", "mean", "n") %in% names(df)))
df0 <- df[df$time == 0, , drop = FALSE]
expect_equal(nrow(df0), 2L)
expect_equal(sort(as.numeric(df0$mean)), c(0, 10))
