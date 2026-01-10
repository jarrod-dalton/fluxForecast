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
#' - Variable type is taken from ps_forecast$meta$schema[[var]]$type.
#' - No type inference from observed values is performed.
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
  categorical_max_levels = 50,
  by = c("run", "patient", "patient_draw")
) {
  validate_ps_forecast(x)

  # Bind data.frames that may have different columns by filling missing columns with NA.
  rbind_fill <- function(a, b) {
    if (is.null(a) || nrow(a) == 0) return(b)
    if (is.null(b) || nrow(b) == 0) return(a)
    a_names <- names(a)
    b_names <- names(b)
    ord <- c(a_names, setdiff(b_names, a_names))
    for (nm in setdiff(ord, a_names)) a[[nm]] <- NA
    for (nm in setdiff(ord, b_names)) b[[nm]] <- NA
    a <- a[, ord, drop = FALSE]
    b <- b[, ord, drop = FALSE]
    rbind(a, b)
  }



  by <- match.arg(by)

  ri <- x$run_index
  if (is.null(ri) || !is.data.frame(ri)) {
    stop("ps_forecast is missing run_index metadata.", call. = FALSE)
  }

  # Determine grouping of runs.
  # IMPORTANT: use split() on row indices (not string keys) to avoid subtle
  # coercion issues (e.g., integer/factor patient_id) that can collapse groups.
  if (by == "run") {
    group_rows <- as.list(seq_len(nrow(ri)))
    names(group_rows) <- as.character(seq_len(nrow(ri)))
    group_type <- "run"
  } else if (by == "patient") {
    if (!("patient_id" %in% names(ri))) {
      stop("run_index is missing patient_id needed for by='patient'.", call. = FALSE)
    }
    group_rows <- split(seq_len(nrow(ri)), as.character(ri$patient_id))
    group_type <- "patient"
  } else {
    if (!("patient_id" %in% names(ri))) {
      stop("run_index is missing patient_id needed for by='patient_draw'.", call. = FALSE)
    }
    if (!("draw_id" %in% names(ri))) {
      stop("run_index is missing draw_id needed for by='patient_draw'.", call. = FALSE)
    }
    key <- paste(as.character(ri$patient_id), as.character(ri$draw_id), sep = "|")
    group_rows <- split(seq_len(nrow(ri)), key)
    group_type <- "patient_draw"
  }

  # Helper: subset a ps_forecast to a set of run indices (rows).
  subset_forecast_runs <- function(x, run_rows) {
    y <- x
    y$run_index <- x$run_index[run_rows, , drop = FALSE]
    y$defined <- x$defined[run_rows, , drop = FALSE]
    y$alive <- x$alive[run_rows, , drop = FALSE]
    y$state <- lapply(x$state, function(m) {
      if (is.null(m)) return(NULL)
      m[run_rows, , drop = FALSE]
    })
    # first_event_time is a [n_runs x n_event_types] matrix
    y$first_event_time <- x$first_event_time[run_rows, , drop = FALSE]
    y
  }

  if (is.null(vars)) vars <- x$vars
  vars <- as.character(vars)

  if (!all(vars %in% x$vars)) {
    bad <- setdiff(vars, x$vars)
    stop("Unknown vars: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  if (is.null(times)) {
    times <- x$times
  } else {
    times <- .psf_as_numeric_time(times, name = "times")
    if (!all(times %in% x$times)) {
      bad <- setdiff(times, x$times)
      stop("times must be a subset of x$times. Unknown: ", paste(bad, collapse = ", "), call. = FALSE)
    }
  }

  Tidx <- match(times, x$times)
    # If grouping by patient (or patient_draw), compute summaries by subsetting runs into groups
  # and reusing the existing by='run' behavior. This avoids subtle row-order assumptions.
  
  # NOTE: by!='run' returns above (via per-group subsetting + by='run' recursion),
  # so the remainder of this function always executes with by='run'.

if (by != "run") {
    out_grp <- stats::setNames(vector("list", length(vars)), vars)
    for (v in vars) out_grp[[v]] <- NULL

    for (g in names(group_rows)) {
      run_rows <- group_rows[[g]]
      xg <- subset_forecast_runs(x, run_rows)
      ssg <- state_summary(
        xg,
        vars = vars,
        times = times,
        categorical_max_levels = categorical_max_levels,
        by = "run"
      )

      # Parse group identifiers to attach columns.
      if (by == "patient") {
        patient_id <- g
        draw_id <- NULL
      } else {
        parts <- strsplit(g, "|", fixed = TRUE)[[1]]
        patient_id <- parts[[1]]
        draw_id <- parts[[2]]
      }

      for (v in vars) {
        dfv <- ssg[[v]]
        if (is.null(dfv) || nrow(dfv) == 0) next
        dfv$patient_id <- patient_id
        if (!is.null(draw_id)) dfv$draw_id <- draw_id
        # Ensure group columns come first.
        if (by == "patient") {
          dfv <- dfv[, c("patient_id", setdiff(names(dfv), "patient_id")), drop = FALSE]
        } else {
          dfv <- dfv[, c("patient_id", "draw_id", setdiff(names(dfv), c("patient_id","draw_id"))), drop = FALSE]
        }
        out_grp[[v]] <- if (is.null(out_grp[[v]])) dfv else rbind_fill(out_grp[[v]], dfv)
      }
    }

    return(out_grp)
  }
out <- stats::setNames(vector("list", length(vars)), vars)

  for (v in vars) {
    mat <- x$state[[v]]
    if (is.null(mat)) {
      out[[v]] <- data.frame(time = times, n = integer(length(times)))
      next
    }

    rows <- list()

    # Determine type (declared in schema; no inference).
    spec <- x$meta$schema[[v]]
    vtype <- as.character(spec$type)
    is_cat <- vtype %in% c("binary", "categorical", "ordinal")
    is_cont <- vtype %in% c("continuous", "count")

    if (is_cont) {
      # Continuous / count: per-time summaries (by='run' only; other groupings
      # are handled above via explicit per-group subsetting + recursion).
      for (k in seq_along(Tidx)) {
        j <- Tidx[[k]]
        elig <- !is.na(x$alive[, j]) & (x$alive[, j] == TRUE)
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
          qs <- stats::quantile(vals, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
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
      # Discrete types (binary/categorical/ordinal): tabulate across declared levels.
      levs <- spec$levels
      if (length(levs) > categorical_max_levels) {
        stop(
          paste0(
            "Too many levels for var '", v, "' (", length(levs), ") per schema. ",
            "Increase categorical_max_levels if needed."
          ),
          call. = FALSE
        )
      }

      for (k in seq_along(Tidx)) {
        j <- Tidx[[k]]
        elig <- !is.na(x$alive[, j]) & (x$alive[, j] == TRUE)
        vals <- mat[elig, j]
        vals <- vals[!is.na(vals)]

        if (length(vals) == 0) {
          # Keep a row per level with n=0 so downstream code can rely on a stable support.
          df <- data.frame(
            time = rep(times[[k]], length(levs)),
            level = levs,
            n = rep(0L, length(levs)),
            p = rep(NA_real_, length(levs)),
            stringsAsFactors = FALSE
          )
          rows[[length(rows) + 1L]] <- df
          next
        }

        f <- factor(as.character(vals), levels = levs, ordered = identical(vtype, "ordinal"))
        tab <- table(f, useNA = "no")
        denom <- sum(tab)
        df <- data.frame(
          time = rep(times[[k]], length(tab)),
          level = names(tab),
          n = as.integer(tab),
          p = if (denom > 0) as.numeric(tab) / denom else rep(NA_real_, length(tab)),
          stringsAsFactors = FALSE
        )
        rows[[length(rows) + 1L]] <- df
      }

      out[[v]] <- do.call(rbind, rows)
    }
  }

  out
}