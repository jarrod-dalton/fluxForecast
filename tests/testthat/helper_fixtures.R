test_entity_schema <- function() {
  list(
    alive = list(
      type = "binary",
      levels = c("0", "1"),
      default = TRUE,
      coerce = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    ),
    active_followup = list(
      type = "binary",
      levels = c("0", "1"),
      default = TRUE,
      coerce = as.logical,
      validate = function(x) length(x) == 1L && (is.na(x) || is.logical(x))
    )
  )
}
