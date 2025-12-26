library(testthat)
library(patientSimCore)
library(patientSimForecast)

make_toy_bundle <- function() {
  propose_events <- function(patient, ctx, ...) {
    phase <- patient$state()["phase"]
    alive <- patient$state()["alive"]
    props <- list()

    # visit every 1 time unit until time < 6
    if (patient$last_time < 6) {
      props$visit <- list(time_next = patient$last_time + 1, event_type = "visit")
    }

    # transplant at time 3 if still on waitlist
    if (isTRUE(alive) && identical(phase, "waitlist") && patient$last_time < 3) {
      props$tx <- list(time_next = 3, event_type = "transplant")
    }

    # death at time 5 if alive
    if (isTRUE(alive) && patient$last_time < 5) {
      props$death <- list(time_next = 5, event_type = "death")
    }

    props
  }

  transition <- function(patient, event, ctx) {
    et <- event$event_type
    if (et == "visit") {
      x <- patient$state()["x"]
      return(list(x = x + 1))
    }
    if (et == "transplant") {
      return(list(phase = "post_tx"))
    }
    if (et == "death") {
      return(list(alive = FALSE))
    }
    NULL
  }

  stop <- function(patient, event, ctx) {
    if (!isTRUE(patient$state()["alive"])) return(TRUE)
    if (patient$last_time >= 6) return(TRUE)
    FALSE
  }

  list(
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    refresh_rules = function(...) "ALL"
  )
}

test_that("forecast -> risk() and survival() behave as expected", {
  schema <- list(
    alive = list(default = TRUE, coerce = as.logical),
    phase = list(default = "A", coerce = as.character),
    x = list(default = 0, coerce = as.numeric)
  )

  p <- Patient$new(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)

  bundle <- make_toy_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    patients = list(p1 = p),
    times = c(0, 1, 3, 4, 6),
    S = 3,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object"
  )

  r_tx <- risk(fx, event = "transplant", start_time = 0)
  expect_equal(r_tx$result$risk[r_tx$result$time == 1], 0)
  expect_equal(r_tx$result$risk[r_tx$result$time == 3], 1)

  s_death <- survival(fx, terminal_events = "death", start_time = 0)
  expect_equal(s_death$result$event_free[s_death$result$time == 4], 1)
  expect_equal(s_death$result$event_free[s_death$result$time == 6], 0)

  # eligibility: waitlist at start_time (should include all)
  elig_wait <- function(snap, time, ctx) isTRUE(as.logical(snap$alive)) && identical(as.character(snap$phase), "waitlist")
  r_tx2 <- risk(fx, event = "transplant", start_time = 0, eligible = elig_wait)
  expect_equal(r_tx2$result$risk[r_tx2$result$time == 3], 1)
})


test_that("draws() returns a data.frame and respects times", {
  schema <- list(
    alive = list(default = TRUE, coerce = as.logical),
    phase = list(default = "A", coerce = as.character),
    x = list(default = 0, coerce = as.numeric)
  )

  p <- Patient$new(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)

  bundle <- make_toy_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    patients = list(p1 = p),
    times = c(0, 1, 3),
    S = 2,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object"
  )

  d <- draws(fx, var = "x", times = c(0, 3))
  expect_true(is.data.frame(d))
  expect_true(all(d$time %in% c(0, 3)))
})
