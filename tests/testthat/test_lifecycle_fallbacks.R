library(testthat)
library(fluxCore)
library(fluxForecast)

make_no_alive_schema <- function() {
  list(
    phase = list(type = "categorical", levels = c("idle", "active"), default = "idle", coerce = as.character),
    x = list(type = "continuous", default = 0, coerce = as.numeric)
  )
}

make_alive_schema <- function() {
  schema <- test_entity_schema()
  schema$x <- list(type = "continuous", default = 0, coerce = as.numeric)
  schema
}

make_terminal_bundle_no_alive <- function() {
  list(
    time_spec = fluxCore::time_spec(unit = "hours"),
    event_catalog = c("visit", "failure"),
    terminal_events = "failure",
    propose_events = function(entity, ctx = NULL, ...) {
      out <- list()
      if (entity$last_time < 1) out$visit <- list(time_next = 1, event_type = "visit")
      if (entity$last_time < 2) out$failure <- list(time_next = 2, event_type = "failure")
      out
    },
    transition = function(entity, event, ctx = NULL) {
      if (identical(event$event_type, "visit")) {
        return(list(phase = "active", x = entity$state()[["x"]] + 1))
      }
      NULL
    },
    stop = function(entity, event, ctx = NULL) identical(event$event_type, "failure"),
    refresh_rules = function(...) "ALL"
  )
}

make_defined_only_bundle_no_alive <- function() {
  list(
    time_spec = fluxCore::time_spec(unit = "hours"),
    propose_events = function(entity, ctx = NULL, ...) {
      out <- list()
      if (entity$last_time < 2) out$visit <- list(time_next = entity$last_time + 1, event_type = "visit")
      out
    },
    transition = function(entity, event, ctx = NULL) {
      list(phase = "active", x = entity$state()[["x"]] + 1)
    },
    stop = function(entity, event, ctx = NULL) entity$last_time >= 2,
    refresh_rules = function(...) "ALL"
  )
}

make_alive_precedence_bundle <- function() {
  list(
    time_spec = fluxCore::time_spec(unit = "hours"),
    event_catalog = c("visit", "failure"),
    terminal_events = "failure",
    propose_events = function(entity, ctx = NULL, ...) {
      out <- list()
      if (entity$last_time < 1) out$visit <- list(time_next = 1, event_type = "visit")
      if (entity$last_time < 2) out$failure <- list(time_next = 2, event_type = "failure")
      out
    },
    transition = function(entity, event, ctx = NULL) {
      if (identical(event$event_type, "visit")) {
        return(list(x = entity$state()[["x"]] + 1))
      }
      # Keep modeled alive unchanged to verify precedence over terminal-events fallback.
      NULL
    },
    stop = function(entity, event, ctx = NULL) identical(event$event_type, "failure"),
    refresh_rules = function(...) "ALL"
  )
}

test_that("forecast derives lifecycle from bundle terminal_events when schema omits alive", {
  schema <- make_no_alive_schema()
  p <- fluxCore::Entity$new(init = list(phase = "idle", x = 0), schema = schema, time0 = 0)

  bundle <- make_terminal_bundle_no_alive()
  provider <- list(load = function(model_spec = NULL, ...) bundle)
  engine <- fluxCore::Engine$new(provider = provider)

  fx <- suppressWarnings(forecast(
    engine = engine,
    entities = list(e1 = p),
    times = c(0, 1, 2, 3),
    S = 1,
    vars = c("phase", "x"),
    backend = "none",
    return = "object"
  ))

  expect_equal(as.logical(fx$alive[1, ]), c(TRUE, TRUE, FALSE, NA))

  s <- survival(fx, terminal_events = "failure", start_time = 0)
  expect_equal(s$result$event_free[s$result$time == 1], 1)
  expect_equal(s$result$event_free[s$result$time == 2], 0)
})

test_that("forecast falls back to lifecycle-active wherever defined when alive and terminal_events are absent", {
  schema <- make_no_alive_schema()
  p <- fluxCore::Entity$new(init = list(phase = "idle", x = 0), schema = schema, time0 = 0)

  bundle <- make_defined_only_bundle_no_alive()
  provider <- list(load = function(model_spec = NULL, ...) bundle)
  engine <- fluxCore::Engine$new(provider = provider)

  fx <- suppressWarnings(forecast(
    engine = engine,
    entities = list(e1 = p),
    times = c(0, 1, 2, 3),
    S = 1,
    vars = c("phase", "x"),
    backend = "none",
    return = "object"
  ))

  expect_equal(as.logical(fx$alive[1, ]), c(TRUE, TRUE, TRUE, NA))
})

test_that("modeled alive takes precedence over bundle terminal_events fallback", {
  schema <- make_alive_schema()
  p <- fluxCore::Entity$new(init = list(alive = TRUE, active_followup = TRUE, x = 0), schema = schema, time0 = 0)

  bundle <- make_alive_precedence_bundle()
  provider <- list(load = function(model_spec = NULL, ...) bundle)
  engine <- fluxCore::Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    entities = list(e1 = p),
    times = c(0, 1, 2),
    S = 1,
    vars = c("x"),
    backend = "none",
    return = "object"
  )

  expect_equal(as.logical(fx$alive[1, ]), c(TRUE, TRUE, TRUE))
})

test_that("streaming summaries work when alive is omitted and terminal_events are declared", {
  schema <- make_no_alive_schema()
  p <- fluxCore::Entity$new(init = list(phase = "idle", x = 0), schema = schema, time0 = 0)

  bundle <- make_terminal_bundle_no_alive()
  provider <- list(load = function(model_spec = NULL, ...) bundle)
  engine <- fluxCore::Engine$new(provider = provider)

  ep <- suppressWarnings(event_prob_forecast(
    engine = engine,
    entities = list(e1 = p),
    times = c(0, 1, 2),
    event = "visit",
    S = 1,
    backend = "none"
  ))

  expect_s3_class(ep, "flux_event_prob")
  expect_equal(ep$result$event_prob[ep$result$time == 1], 1)

  ss <- suppressWarnings(state_summary_forecast(
    engine = engine,
    entities = list(e1 = p),
    times = c(0, 1, 2),
    vars = "x",
    S = 1,
    backend = "none"
  ))
  expect_true(is.list(ss))
  expect_true(is.list(ss$numeric))
  expect_true("x" %in% names(ss$numeric))
})
