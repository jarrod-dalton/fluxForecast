test_that("forecast warning suppression does not hide model callback errors", {
  model_time <- fluxCore::time_spec(unit = "hours")
  schema <- fluxCore::set_schema(
    vars = list(
      alive = list(
        type = "binary",
        levels = c("0", "1"),
        default = TRUE,
        coerce = as.logical,
        validate = function(x) {
          length(x) == 1L && is.logical(x) && !is.na(x)
        }
      ),
      deliveries = list(
        type = "nonnegative_integer",
        default = 0L
      )
    ),
    time_spec = model_time
  )
  bundle <- list(
    time_spec = model_time,
    event_catalog = "delivery",
    propose_events = function(entity) {
      list(delivery = list(time_next = 1, event_type = "delivery"))
    },
    transition = function(entity, event) {
      stop("forecast transition sentinel", call. = FALSE)
    },
    stop = function(entity, event) TRUE
  )
  engine <- fluxCore::load_model(schema = schema, bundle = bundle)
  courier <- fluxCore::Entity$new(
    init = list(alive = TRUE, deliveries = 0L),
    schema = schema$variables,
    id = "courier_1"
  )

  expect_error(
    forecast(
      engine = engine,
      entities = list(courier_1 = courier),
      times = c(0, 1),
      S = 1L,
      vars = "deliveries",
      seed = 11L,
      backend = "none"
    ),
    "forecast transition sentinel",
    fixed = TRUE
  )
})
