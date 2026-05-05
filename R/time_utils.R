# Time validation helpers for fluxForecast.
# v2.0: time_spec is required; ctx fallback removed.

.fluxf_as_numeric_time <- function(x, name = "time", time_spec = NULL) {
  if (inherits(x, "Date") || inherits(x, c("POSIXct", "POSIXt"))) {
    unit <- if (!is.null(time_spec) && inherits(time_spec, "time_spec")) {
      as.character(time_spec$unit)
    } else {
      "unitless"
    }
    stop(
      sprintf(
        "%s must be numeric model time (unit: '%s'). Calendar time inputs are out of scope for fluxForecast.",
        name, unit
      ),
      call. = FALSE
    )
  }

  if (is.numeric(x)) return(as.numeric(x))

  y <- suppressWarnings(as.numeric(x))
  if (length(y) != length(x)) {
    unit <- if (!is.null(time_spec) && inherits(time_spec, "time_spec")) {
      as.character(time_spec$unit)
    } else {
      "unitless"
    }
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  if (any(is.na(y) & !is.na(x))) {
    unit <- if (!is.null(time_spec) && inherits(time_spec, "time_spec")) {
      as.character(time_spec$unit)
    } else {
      "unitless"
    }
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  y
}
