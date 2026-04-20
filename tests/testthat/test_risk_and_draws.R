library(testthat)
library(fluxCore)
library(fluxForecast)

make_toy_bundle <- function() {
  propose_events <- function(entity, ctx, ...) {
    phase <- entity$state()[["phase"]]
    alive <- entity$state()[["alive"]]
    props <- list()

    # visit every 1 time unit until time < 6
    if (entity$last_time < 6) {
      props$visit <- list(time_next = entity$last_time + 1, event_type = "visit")
    }

    # transplant at time 3 if still on waitlist
    if (isTRUE(alive) && identical(phase, "waitlist") && entity$last_time < 3) {
      props$tx <- list(time_next = 3, event_type = "transplant")
    }

    # death at time 5 if alive
    if (isTRUE(alive) && entity$last_time < 5) {
      props$death <- list(time_next = 5, event_type = "death")
    }

    props
  }

  transition <- function(entity, event, ctx) {
    et <- event$event_type
    if (et == "visit") {
      x <- entity$state()[["x"]]
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

  stop <- function(entity, event, ctx) {
    if (!isTRUE(entity$state()[["alive"]])) return(TRUE)
    if (entity$last_time >= 6) return(TRUE)
    FALSE
  }

  list(
    propose_events = propose_events,
    transition = transition,
    stop = stop,
    refresh_rules = function(...) "ALL"
  )
}

test_that("forecast -> event_prob() and survival() behave as expected", {
  schema <- fluxCore::default_entity_schema()
  schema[["phase"]] <- list(type = "categorical", levels = c("waitlist","post_mi"), default = "waitlist", coerce = as.character)
  schema[["x"]] <- list(type = "continuous", default = 0, coerce = as.numeric, validate = function(v) length(v) == 1L && is.finite(v))

  p <- fluxCore::new_entity(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)

  bundle <- make_toy_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    entities = list(p1 = p),
    times = c(0, 1, 3, 4, 6),
    S = 3,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object"
  )

  r_tx <- event_prob(fx, event = "transplant", start_time = 0)
  expect_equal(r_tx$result$event_prob[r_tx$result$time == 1], 0)
  expect_equal(r_tx$result$event_prob[r_tx$result$time == 3], 1)

  s_death <- survival(fx, terminal_events = "death", start_time = 0)
  expect_equal(s_death$result$event_free[s_death$result$time == 4], 1)
  expect_equal(s_death$result$event_free[s_death$result$time == 6], 0)

  # eligibility: waitlist at start_time (should include all)
  elig_wait <- function(snap, time, ctx) isTRUE(as.logical(snap$alive)) && identical(as.character(snap$phase), "waitlist")
  r_tx2 <- event_prob(fx, event = "transplant", start_time = 0, eligible = elig_wait)
  expect_equal(r_tx2$result$event_prob[r_tx2$result$time == 3], 1)
})


test_that("draws() returns a data.frame and respects times", {
  schema <- list(
    alive = list(type = "binary", levels = c("FALSE","TRUE"), default = TRUE, coerce = as.logical),
    phase = list(type = "categorical", levels = c("waitlist","transplanted","A","B"), default = "A", coerce = as.character),
    x = list(type = "continuous", default = 0, coerce = as.numeric)
  )

  p <- fluxCore::new_entity(init = list(alive = TRUE, phase = "waitlist", x = 0), schema = schema, time0 = 0)

  bundle <- make_toy_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  fx <- forecast(
    engine = engine,
    entities = list(p1 = p),
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
