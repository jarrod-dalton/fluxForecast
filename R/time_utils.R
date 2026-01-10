# ------------------------------------------------------------------------------
# Numeric model-time validation helpers
# ------------------------------------------------------------------------------

.psf_time_unit_label <- function(ctx = NULL) {
  # Forecast uses model-time numeric. Units are a modeling convention declared in ctx$time$unit.
  if (!is.null(ctx) && is.list(ctx)) {
    if (!is.null(ctx$time) && is.list(ctx$time)) {
      u <- ctx$time$unit
      if (is.character(u) && length(u) == 1L && nzchar(u)) return(as.character(u))
    }
    # ctx may be a list of per-draw ctx lists
    if (length(ctx) > 0L && all(vapply(ctx, is.list, logical(1)))) {
      for (i in seq_along(ctx)) {
        u <- tryCatch(.psf_time_unit_label(ctx[[i]]), error = function(e) NULL)
        if (!is.null(u)) return(u)
      }
    }
  }
  "unitless"
}

.psf_as_numeric_time <- function(x, name = "time", ctx = NULL) {
  if (inherits(x, "Date") || inherits(x, c("POSIXct", "POSIXt"))) {
    unit <- .psf_time_unit_label(ctx)
    stop(
      sprintf(
        "%s must be numeric model time (unit: '%s'). Calendar time inputs are out of scope for patientSimForecast.",
        name, unit
      ),
      call. = FALSE
    )
  }

  # Keep legacy convenience: allow coercible numerics, but disallow NA introduction.
  if (is.numeric(x)) return(as.numeric(x))

  y <- suppressWarnings(as.numeric(x))
  if (length(y) != length(x)) {
    unit <- .psf_time_unit_label(ctx)
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  if (any(is.na(y) & !is.na(x))) {
    unit <- .psf_time_unit_label(ctx)
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  y
}
