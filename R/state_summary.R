# ------------------------------------------------------------------------------
# state_summary()
# ------------------------------------------------------------------------------

#' Summarize forecasted state variables over time
#'
#' Given a ps_forecast object (from forecast(return="object")), summarize the
#' distribution of state variables at each requested time among runs that are
#' still under active follow-up at that time.
#'
#' Type rules:
#' - logical, character: categorical
#' - numeric/integer with values only in {0,1}: categorical
#' - other numeric: continuous
#'
#' @param x A ps_forecast object.
#' @param vars Character vector of variables to summarize (default: all vars in x).
#' @param times Optional numeric vector subset of x$times (default: all).
#' @param categorical_max_levels Maximum number of distinct levels allowed for categorical vars.
#' @return A named list with one entry per var. Each entry is a data.frame.
#' @export
state_summary <- function(
  x,
  vars = NULL,
  times = NULL,
  categorical_max_levels = 50
) {
  validate_ps_forecast(x)

  if (is.null(vars)) vars <- x$vars
  vars <- as.character(vars)

  if (!all(vars %in% x$vars)) {
    bad <- setdiff(vars, x$vars)
    stop("Unknown vars: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (is.null(times)) {
    times <- x$times
  } else {
    times <- as.numeric(times)
    if (!all(times %in% x$times)) {
      bad <- setdiff(times, x$times)
      stop("times must be a subset of x$times. Unknown: ", paste(bad, collapse = ", "), call. = FALSE)
    }
  }

  Tidx <- match(times, x$times)
  out <- stats::setNames(vector("list", length(vars)), vars)

  for (v in vars) {
    mat <- x$state[[v]]
    if (is.null(mat)) {
      out[[v]] <- data.frame(time = times, n = integer(length(times)))
      next
    }

    # Collect per-time vectors among active follow-up runs.
    rows <- list()
    is_binary_global <- TRUE
    nonmissing_seen <- FALSE

    # We'll decide type per variable using all eligible values across times.
    all_vals <- c()

    for (k in seq_along(Tidx)) {
      j <- Tidx[[k]]
      elig <- isTRUE(x$alive[, j])
      vals <- mat[elig, j]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        nonmissing_seen <- TRUE
        all_vals <- c(all_vals, vals)
      }
    }

    # If nothing observed, return empty-ish numeric summary.
    if (!nonmissing_seen) {
      out[[v]] <- data.frame(time = times, n = integer(length(times)))
      next
    }

    # Determine type
    is_cat <- FALSE
    is_cont <- FALSE

    if (is.logical(all_vals) || is.character(all_vals)) {
      is_cat <- TRUE
    } else if (is.numeric(all_vals)) {
      u <- unique(all_vals[is.finite(all_vals)])
      if (length(u) > 0 && all(u %in% c(0, 1))) {
        is_cat <- TRUE
      } else {
        is_cont <- TRUE
      }
    } else {
      # fallback: treat as categorical
      is_cat <- TRUE
    }

    if (is_cont) {
      for (k in seq_along(Tidx)) {
        j <- Tidx[[k]]
        elig <- isTRUE(x$alive[, j])
        vals <- mat[elig, j]
        vals <- vals[!is.na(vals)]
        vals <- suppressWarnings(as.numeric(vals))
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0) {
          rows[[k]] <- data.frame(
            time = times[[k]],
            n = 0L,
            mean = NA_real_,
            sd = NA_real_,
            min = NA_real_,
            q1 = NA_real_,
            median = NA_real_,
            q3 = NA_real_,
            max = NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          qs <- as.numeric(stats::quantile(vals, probs = c(0.25, 0.5, 0.75), names = FALSE, type = 7))
          rows[[k]] <- data.frame(
            time = times[[k]],
            n = as.integer(length(vals)),
            mean = mean(vals),
            sd = if (length(vals) > 1) stats::sd(vals) else NA_real_,
            min = min(vals),
            q1 = qs[[1]],
            median = qs[[2]],
            q3 = qs[[3]],
            max = max(vals),
            stringsAsFactors = FALSE
          )
        }
      }
      out[[v]] <- do.call(rbind, rows)
    } else if (is_cat) {
      # counts/proportions by level at each time
      for (k in seq_along(Tidx)) {
        j <- Tidx[[k]]
        elig <- isTRUE(x$alive[, j])
        vals <- mat[elig, j]
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) next
        lev <- as.character(vals)
        tab <- table(lev, useNA = "no")
        if (length(tab) > categorical_max_levels) {
          stop("Too many levels for categorical var '", v, "' (", length(tab), "). Increase categorical_max_levels if needed.", call. = FALSE)
        }
        df <- data.frame(
          time = rep(times[[k]], length(tab)),
          level = names(tab),
          n = as.integer(tab),
          p = as.numeric(tab) / sum(tab),
          stringsAsFactors = FALSE
        )
        rows[[length(rows) + 1L]] <- df
      }
      out[[v]] <- if (length(rows) == 0L) {
        data.frame(time = numeric(0), level = character(0), n = integer(0), p = numeric(0), stringsAsFactors = FALSE)
      } else {
        do.call(rbind, rows)
      }
    }
  }

  out
}
