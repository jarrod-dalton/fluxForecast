# ------------------------------------------------------------------------------
# Internal utilities (not exported)
# ------------------------------------------------------------------------------

norm_bool <- function(x) {
  if (is.logical(x)) return(x)
  suppressWarnings(as.logical(x))
}

# NA-safe, vectorized TRUE mapping: returns TRUE/FALSE with NA preserved.
na_safe_true <- function(x) {
  x <- norm_bool(x)
  ifelse(is.na(x), NA, x %in% TRUE)
}

# NA-safe, scalar TRUE mapping: returns TRUE/FALSE with NA preserved.
na_safe_true1 <- function(x) {
  if (is.na(x)) return(NA)
  identical(x, TRUE)
}

# Scalar TRUE check (no NA handling).
is_true1 <- function(x) identical(x, TRUE)

isTRUEorNA <- function(x) {
  na_safe_true(x)
}

.fluxf_internal_env <- new.env(parent = emptyenv())

.fluxf_warn_once <- function(key, msg) {
  if (!is.character(key) || length(key) != 1L) key <- "__default__"
  if (isTRUE(.fluxf_internal_env[[key]])) return(invisible(FALSE))
  .fluxf_internal_env[[key]] <- TRUE
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

.fluxf_bundle_event_set <- function(bundle, name) {
  if (is.null(bundle) || !is.list(bundle) || is.null(bundle[[name]])) return(NULL)
  x <- unique(as.character(bundle[[name]]))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) return(NULL)
  x
}

.fluxf_bundle_event_catalog <- function(bundle) {
  .fluxf_bundle_event_set(bundle, "event_catalog")
}

.fluxf_bundle_terminal_events <- function(bundle) {
  .fluxf_bundle_event_set(bundle, "terminal_events")
}

.fluxf_first_event_time_any <- function(events_df, event_set) {
  if (is.null(event_set) || length(event_set) == 0L) return(Inf)
  if (is.null(events_df) || nrow(events_df) == 0L) return(Inf)
  et <- as.character(events_df$event_type)
  keep <- et %in% event_set
  if (!any(keep)) return(Inf)
  min(as.numeric(events_df$time[keep]))
}
