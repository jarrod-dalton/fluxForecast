test_that("forecast batch execution does not hide decision callback errors", {
  model_time <- fluxCore::time_spec(unit = "hours")
  dispatch_review <- fluxCore::DecisionPoint(
    id = "dispatch_review",
    trigger = "delivery",
    condition = function(entity) {
      stop("forecast decision condition sentinel", call. = FALSE)
    }
  )
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
    time_spec = model_time,
    decision_points = list(dispatch_review)
  )
  bundle <- list(
    time_spec = model_time,
    event_catalog = "delivery",
    propose_events = function(entity) {
      list(delivery = list(time_next = 1, event_type = "delivery"))
    },
    transition = function(entity, event) {
      list(deliveries = entity$current$deliveries + 1L)
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
    "DecisionPoint('dispatch_review') condition callback errored: forecast decision condition sentinel",
    fixed = TRUE
  )
})
