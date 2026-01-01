library(testthat)
library(patientSimCore)
library(patientSimForecast)

make_toy_bundle <- function() {
  propose_events <- function(patient, ctx, ...) {
    props <- list()
    if (patient$last_time < 2) {
      props$visit <- list(time_next = patient$last_time + 1, event_type = "visit")
    }
    props
  }

  transition <- function(patient, event, ctx, ...) {
    if (event$event_type == "visit") {
      # no-op
      NULL
    } else {
      NULL
    }
  }

  stop <- function(patient, event, ctx, ...) {
    patient$last_time >= 2
  }

  list(
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    refresh_rules = function(...) "ALL"
  )
}

test_that("state_summary supports by='patient'", {
  schema <- list(
    alive = list(type = "binary", levels = c("FALSE","TRUE"), default = TRUE, coerce = as.logical),
    phase = list(type = "categorical", levels = c("waitlist","A","B"), default = "A", coerce = as.character),
    x = list(type = "continuous", default = 0, coerce = as.numeric)
  )

  p1 <- patientSimCore::new_patient(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)
  p2 <- patientSimCore::new_patient(init = list(alive = TRUE, phase = "waitlist", x = 10), schema = schema, time0 = 0)

  bundle <- make_toy_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    patients = list(p1 = p1, p2 = p2),
    times = c(0, 1, 2),
    S = 2,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object"
  )

  ss <- state_summary(fx, vars = "x", times = c(0), by = "patient")
  expect_true(is.list(ss))
  expect_true("x" %in% names(ss))
  df <- ss[["x"]]
  expect_true(all(c("patient_id", "time", "mean", "n") %in% names(df)))

    # time 0 should reflect initial x values within each patient (labels may be numeric or names)
  df0 <- df[df$time == 0, , drop = FALSE]
  expect_equal(nrow(df0), 2L)
  expect_equal(sort(as.numeric(df0$mean)), c(0, 10))
})

