# DEPRECATED v1.x helpers (scheduled for removal; use time_spec instead):
#   .fluxf_ctx_is_per_draw, .fluxf_ctx_for_draw
#
# These helpers are retained temporarily for back-compat with code that still uses
# v1.x ctx patterns. They will be removed in a future release.
#
# For new code: use formal time_spec, ParamContext, and SimContext instead.
#
# v2.0 migration: Replace ctx-based param passing with ParamContext lists;
# replace per-draw ctx extraction with ParamContext indexing.

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

# v2.0 time validation: time_spec is required; ctx fallback removed.
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
