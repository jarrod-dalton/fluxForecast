# ------------------------------------------------------------------------------
# Numeric model-time validation helpers
# ------------------------------------------------------------------------------

.fluxf_time_unit_label <- function(ctx = NULL, time_spec = NULL) {
  if (!is.null(time_spec) && inherits(time_spec, "time_spec")) {
    return(as.character(time_spec$unit))
  }
  # Forecast uses model-time numeric. Units are a modeling convention declared in ctx$time$unit.
  if (!is.null(ctx) && is.list(ctx)) {
    if (!is.null(ctx$time) && is.list(ctx$time)) {
      u <- ctx$time$unit
      if (is.character(u) && length(u) == 1L && nzchar(u)) return(as.character(u))
    }
    # ctx may be a list of per-draw ctx lists
    if (length(ctx) > 0L && all(vapply(ctx, is.list, logical(1)))) {
      for (i in seq_along(ctx)) {
        u <- tryCatch(.fluxf_time_unit_label(ctx[[i]], time_spec = NULL), error = function(e) NULL)
        if (!is.null(u) && !identical(u, "unitless")) return(u)
      }
    }
  }
  "unitless"
}

.fluxf_ctx_is_per_draw <- function(ctx) {
  if (is.null(ctx) || !is.list(ctx) || length(ctx) == 0L) return(FALSE)
  if (!all(vapply(ctx, is.list, logical(1)))) return(FALSE)
  nms <- names(ctx)
  if (!is.null(nms)) {
    reserved <- c("time", "time_spec", "params", "entity_id", "param_draw_id", "sim_id")
    if (any(nzchar(nms) & nms %in% reserved)) return(FALSE)
  }
  TRUE
}

.fluxf_ctx_for_draw <- function(ctx, param_draw_id, n_param_sets) {
  if (is.null(ctx)) return(NULL)
  is_per_draw_ctx <- .fluxf_ctx_is_per_draw(ctx)
  if (!is_per_draw_ctx) return(ctx)
  if (length(ctx) != n_param_sets) {
    stop("If ctx is a list of lists, it must have length equal to number of parameter sets.", call. = FALSE)
  }
  ctx[[param_draw_id]]
}

.fluxf_as_numeric_time <- function(x, name = "time", ctx = NULL, time_spec = NULL) {
  if (inherits(x, "Date") || inherits(x, c("POSIXct", "POSIXt"))) {
    unit <- .fluxf_time_unit_label(ctx, time_spec = time_spec)
    stop(
      sprintf(
        "%s must be numeric model time (unit: '%s'). Calendar time inputs are out of scope for fluxForecast.",
        name, unit
      ),
      call. = FALSE
    )
  }

  # Keep legacy convenience: allow coercible numerics, but disallow NA introduction.
  if (is.numeric(x)) return(as.numeric(x))

  y <- suppressWarnings(as.numeric(x))
  if (length(y) != length(x)) {
    unit <- .fluxf_time_unit_label(ctx, time_spec = time_spec)
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  if (any(is.na(y) & !is.na(x))) {
    unit <- .fluxf_time_unit_label(ctx, time_spec = time_spec)
    stop(sprintf("%s must be numeric model time (unit: '%s').", name, unit), call. = FALSE)
  }
  y
}
