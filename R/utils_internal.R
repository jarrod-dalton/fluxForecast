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
