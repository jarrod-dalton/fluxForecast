library(testthat)
library(patientSimCore)
library(patientSimForecast)

test_that("forecast rejects calendar-time inputs", {
  bundle <- list(
    propose_events = function(patient, ctx, ...) {
      if (patient$last_time < 1) {
        return(list(visit = list(time_next = patient$last_time + 1, event_type = "visit")))
      }
      list()
    },
    transition = function(patient, event, ctx) list(),
    stop = function(patient, event, ctx) TRUE,
    refresh_rules = function(...) "ALL"
  )

  engine <- Engine$new(provider = list(load = function(model_spec, ...) bundle))
  p <- patientSimCore::new_patient(init = list(alive = TRUE), schema = patientSimCore::default_patient_schema(), time0 = 0)

  expect_error(
    forecast(engine = engine, patients = list(p1 = p), times = as.Date(c("2000-01-01", "2000-01-02")), S = 1, backend = "none", return = "none"),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})

test_that("risk/draws reject calendar-time inputs", {
  bundle <- list(
    propose_events = function(patient, ctx, ...) {
      if (patient$last_time < 1) {
        return(list(visit = list(time_next = patient$last_time + 1, event_type = "visit")))
      }
      list()
    },
    transition = function(patient, event, ctx) list(),
    stop = function(patient, event, ctx) TRUE,
    refresh_rules = function(...) "ALL"
  )

  engine <- Engine$new(provider = list(load = function(model_spec, ...) bundle))
  p <- patientSimCore::new_patient(init = list(alive = TRUE), schema = patientSimCore::default_patient_schema(), time0 = 0)

  fx <- forecast(engine = engine, patients = list(p1 = p), times = c(0, 1), S = 1, backend = "none", return = "object", ctx = list(time = list(unit = "days")))

  expect_error(
    risk(fx, event = "visit", start_time = as.Date("2000-01-01")),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )

  expect_error(
    draws(fx, var = "alive", times = as.Date(c("2000-01-01", "2000-01-02"))),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})
