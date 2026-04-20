library(testthat)
library(fluxCore)
library(fluxForecast)

test_that("forecast rejects calendar-time inputs", {
  bundle <- list(
    propose_events = function(entity, ctx, ...) {
      if (entity$last_time < 1) {
        return(list(visit = list(time_next = entity$last_time + 1, event_type = "visit")))
      }
      list()
    },
    transition = function(entity, event, ctx) list(),
    stop = function(entity, event, ctx) TRUE,
    refresh_rules = function(...) "ALL"
  )

  engine <- Engine$new(provider = list(load = function(model_spec, ...) bundle))
  p <- fluxCore::new_entity(init = list(alive = TRUE), schema = fluxCore::default_entity_schema(), time0 = 0)

  expect_error(
    forecast(engine = engine, entities = list(p1 = p), times = as.Date(c("2000-01-01", "2000-01-02")), S = 1, backend = "none", return = "none"),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})

test_that("event_prob/draws reject calendar-time inputs", {
  bundle <- list(
    propose_events = function(entity, ctx, ...) {
      if (entity$last_time < 1) {
        return(list(visit = list(time_next = entity$last_time + 1, event_type = "visit")))
      }
      list()
    },
    transition = function(entity, event, ctx) list(),
    stop = function(entity, event, ctx) TRUE,
    refresh_rules = function(...) "ALL"
  )

  engine <- Engine$new(provider = list(load = function(model_spec, ...) bundle))
  p <- fluxCore::new_entity(init = list(alive = TRUE), schema = fluxCore::default_entity_schema(), time0 = 0)

  fx <- forecast(engine = engine, entities = list(p1 = p), times = c(0, 1), S = 1, backend = "none", return = "object", ctx = list(time = list(unit = "days")))

  expect_error(
    event_prob(fx, event = "visit", start_time = as.Date("2000-01-01")),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )

  expect_error(
    draws(fx, var = "alive", times = as.Date(c("2000-01-01", "2000-01-02"))),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})
