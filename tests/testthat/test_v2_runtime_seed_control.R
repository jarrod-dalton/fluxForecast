.seed_control_fixture <- function(stored_seed = 8675309L) {
  model_time <- fluxCore::time_spec(unit = "hours")
  schema <- fluxCore::set_schema(
    vars = list(
      alive = list(
        type = "binary",
        levels = c("0", "1"),
        default = TRUE,
        coerce = as.logical,
        validate = function(x) length(x) == 1L && is.logical(x) && !is.na(x)
      ),
      x = list(
        type = "numeric",
        default = 0,
        coerce = as.numeric,
        validate = function(x) length(x) == 1L && is.numeric(x) && is.finite(x)
      )
    ),
    time_spec = model_time
  )

  bundle <- list(
    time_spec = model_time,
    event_catalog = c("hit", "miss"),
    propose_events = function(entity, param_ctx = NULL) {
      if (entity$last_time >= 1) return(list())
      event_type <- if (stats::runif(1) < 0.5) "hit" else "miss"
      list(pulse = list(time_next = 1, event_type = event_type))
    },
    transition = function(entity, event, param_ctx = NULL) {
      offset <- param_ctx$params$offset
      if (is.null(offset)) offset <- 0
      list(x = offset + stats::runif(1))
    },
    stop = function(entity, event) TRUE
  )

  list(
    engine = fluxCore::load_model(
      schema = schema,
      bundle = bundle,
      runtime = fluxCore::RuntimeContext(seed = stored_seed)
    ),
    entity = fluxCore::Entity$new(
      init = list(alive = TRUE, x = 0),
      schema = schema$variables,
      id = "courier_1",
      time0 = 0
    )
  )
}

.run_seeded_forecast <- function(fixture, seed, S = 8L, param_sets = list(list(offset = 0))) {
  forecast(
    engine = fixture$engine,
    entities = list(courier_1 = fixture$entity),
    times = c(0, 1),
    S = S,
    param_sets = param_sets,
    vars = c("alive", "x"),
    seed = seed,
    backend = "none"
  )
}

test_that("forecast keeps bare parameter payloads at its public boundary", {
  fixture <- .seed_control_fixture()
  fx <- .run_seeded_forecast(
    fixture,
    seed = 101L,
    S = 1L,
    param_sets = list(list(offset = 0), list(offset = 10))
  )

  expect_equal(fx$run_index$param_draw_id, 1:2)
  x_at_one <- fx$state$x[, fx$times == 1]
  expect_true(x_at_one[[1]] >= 0 && x_at_one[[1]] < 1)
  expect_true(x_at_one[[2]] >= 10 && x_at_one[[2]] < 11)
})

test_that("forecast seed overrides a loaded Engine runtime seed", {
  fixture <- .seed_control_fixture(stored_seed = 24680L)

  first <- .run_seeded_forecast(fixture, seed = 100L)
  repeat_first <- .run_seeded_forecast(fixture, seed = 100L)
  second <- .run_seeded_forecast(fixture, seed = 200L)

  expect_identical(first$first_event_time, repeat_first$first_event_time)
  expect_identical(first$state$x, repeat_first$state$x)
  expect_false(identical(first$state$x, second$state$x))

  x_at_one <- first$state$x[, first$times == 1]
  expect_gt(length(unique(x_at_one)), 1L)
})

test_that("event_prob_forecast seed overrides a loaded Engine runtime seed", {
  fixture <- .seed_control_fixture(stored_seed = 24680L)
  run_summary <- function(seed) {
    event_prob_forecast(
      engine = fixture$engine,
      entities = list(courier_1 = fixture$entity),
      times = c(0, 1),
      event = "hit",
      S = 8L,
      param_sets = list(list(offset = 0)),
      seed = seed,
      backend = "none"
    )
  }

  first <- run_summary(100L)
  repeat_first <- run_summary(100L)
  second <- run_summary(200L)

  expect_identical(first$result, repeat_first$result)
  expect_false(identical(first$result$event_prob, second$result$event_prob))

  at_one <- first$result[first$result$time == 1, , drop = FALSE]
  expect_gt(at_one$n_events, 0L)
  expect_lt(at_one$n_events, at_one$n_eligible)
})

test_that("state_summary_forecast seed overrides a loaded Engine runtime seed", {
  fixture <- .seed_control_fixture(stored_seed = 24680L)
  run_summary <- function(seed) {
    state_summary_forecast(
      engine = fixture$engine,
      entities = list(courier_1 = fixture$entity),
      times = c(0, 1),
      vars = "x",
      S = 8L,
      param_sets = list(list(offset = 0)),
      seed = seed,
      backend = "none"
    )
  }

  first <- run_summary(100L)
  repeat_first <- run_summary(100L)
  second <- run_summary(200L)

  expect_identical(first$numeric$x, repeat_first$numeric$x)
  expect_false(identical(first$numeric$x$mean, second$numeric$x$mean))

  at_one <- first$numeric$x[first$numeric$x$time == 1, , drop = FALSE]
  expect_gt(at_one$max, at_one$min)
})
