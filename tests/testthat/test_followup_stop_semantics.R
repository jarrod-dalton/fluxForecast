library(testthat)
library(fluxCore)
library(fluxForecast)

make_stop_bundle <- function() {
  propose_events <- function(entity, ctx, ...) {
    phase <- entity$state()[["phase"]]
    alive <- entity$state()[["alive"]]
    props <- list()

    # One visit at time 1 to mutate x.
    if (isTRUE(alive) && entity$last_time < 1) {
      props$visit <- list(time_next = 1, event_type = "visit")
    }

    # Transplant at time 2 if still on waitlist.
    if (isTRUE(alive) && identical(phase, "waitlist") && entity$last_time < 2) {
      props$tx <- list(time_next = 2, event_type = "transplant")
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
      # Follow-up ends at transplant, but entity is still alive.
      x <- entity$state()[["x"]]
      return(list(phase = "post_tx", x = x + 10))
    }
    NULL
  }

  stop <- function(entity, event, ctx) {
    # Stop follow-up at transplant (non-death).
    if (!is.null(event) && identical(event$event_type, "transplant")) return(TRUE)
    FALSE
  }

  list(
    time_spec = fluxCore::time_spec(unit = "days"),
    propose_events = propose_events,
    transition = transition,
    stop = stop
  )
}

test_that("follow-up can stop without implying death (alive vs defined)", {
  schema <- test_entity_schema()
  schema[["phase"]] <- list(type = "categorical", levels = c("waitlist","post_mi"), default = "waitlist", coerce = as.character)
  schema[["x"]] <- list(type = "continuous", default = 0, coerce = as.numeric, validate = function(v) length(v) == 1L && is.finite(v))

  p <- fluxCore::Entity$new(
    init = list(alive = TRUE, phase = "waitlist", x = 0),
    schema = schema,
    time0 = 0
  )

  bundle <- make_stop_bundle()
  provider <- list(load = function(model_spec, ...) bundle)
  engine <- Engine$new(provider = provider)

  times <- c(0, 1, 2, 3)

  fx <- forecast(
    engine = engine,
    entities = list(p1 = p),
    times = times,
    S = 1,
    param_sets = list(list()),
    vars = c("alive", "phase", "x"),
    backend = "none",
    return = "object",
    seed = 123
  )

  # One run
  expect_equal(nrow(fx$run_index), 1L)

  # Defined through transplant time, undefined after.
  expect_true(fx$defined[1, which(times == 2)])
  expect_false(fx$defined[1, which(times == 3)])

  # Alive is TRUE at transplant time, and NA once undefined.
  expect_true(isTRUE(fx$alive[1, which(times == 2)]))
  expect_true(is.na(fx$alive[1, which(times == 3)]))

  # State after follow-up stop is NA (not observed/defined).
  expect_true(is.na(fx$state[["x"]][1, which(times == 3)]))

  # state_summary conditions on being alive (and thus implicitly defined): n=0 at time 3.
  ss <- state_summary(fx, vars = "x", times = times)
  row3 <- ss$x[ss$x$time == 3, ]
  expect_equal(row3$n, 0L)
  expect_true(is.na(row3$mean))
})
