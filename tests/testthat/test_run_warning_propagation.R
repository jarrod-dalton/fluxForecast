test_that("forecast batch execution exposes pending-action warnings", {
  model_time <- fluxCore::time_spec(unit = "hours")
  dispatch_review <- fluxCore::DecisionPoint(
    id = "dispatch_review",
    trigger = "delivery_offer",
    allowed_actions = "accept_offer",
    action_handlers = list(
      accept_offer = function(entity, event) list()
    )
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
      offers_received = list(
        type = "nonnegative_integer",
        default = 0L
      )
    ),
    time_spec = model_time,
    decision_points = list(dispatch_review)
  )
  bundle <- list(
    time_spec = model_time,
    event_catalog = c("delivery_offer", "accept_offer"),
    propose_events = function(entity) {
      offers_received <- as.integer(entity$current$offers_received)
      if (offers_received >= 2L) return(list())

      list(offer = list(
        time_next = offers_received + 1,
        event_type = "delivery_offer"
      ))
    },
    transition = function(entity, event) {
      list(offers_received = entity$current$offers_received + 1L)
    },
    stop = function(entity, event) {
      identical(event$event_type, "accept_offer")
    }
  )
  policy <- list(
    propose_action = function(decision_point, entity) {
      fluxCore::ActionEvent(
        action_type = "accept_offer",
        time_next = entity$last_time + 5
      )
    }
  )
  engine <- fluxCore::load_model(
    schema = schema,
    bundle = bundle,
    policy = policy
  )
  courier <- fluxCore::Entity$new(
    init = list(alive = TRUE, offers_received = 0L),
    schema = schema$variables,
    id = "courier_1"
  )

  expect_warning(
    forecast_result <- forecast(
      engine = engine,
      entities = list(courier_1 = courier),
      times = c(0, 7),
      S = 1L,
      vars = "offers_received",
      seed = 11L,
      backend = "none"
    ),
    "replaced a still-pending action",
    fixed = TRUE
  )
  expect_s3_class(forecast_result, "flux_forecast")
})
