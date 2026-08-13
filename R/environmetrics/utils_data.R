###############################################################################
# Data utilities (non-semantic helpers)
# Inputs:
#   - Numeric vectors/matrices
# Outputs:
#   - Standardized values + summary stats
# Dependencies:
#   - Base R
#   - Optional: matrixStats (fast quantiles)
###############################################################################

standardize_with_sd <- function(x, sd_val) {
  list(values = x / sd_val, sd = sd_val)
}

standardize_matrix_cols <- function(mat) {
  sds <- apply(mat, 2, sd)
  list(values = sweep(mat, 2, sds, FUN = "/"), sds = sds)
}

fast_row_quantiles_t <- function(mat, probs, type = 7L, na.rm = FALSE) {
  if (!is.matrix(mat)) {
    mat <- as.matrix(mat)
  }

  if (requireNamespace("matrixStats", quietly = TRUE)) {
    q <- matrixStats::rowQuantiles(
      mat,
      probs = probs,
      na.rm = na.rm,
      type = type,
      digits = 7L,
      useNames = TRUE,
      drop = TRUE
    )
    return(t(q))
  }

  apply(mat, 1, quantile, probs = probs, na.rm = na.rm, type = type)
}

fast_col_quantiles_t <- function(mat, probs, type = 7L, na.rm = FALSE) {
  if (!is.matrix(mat)) {
    mat <- as.matrix(mat)
  }

  if (requireNamespace("matrixStats", quietly = TRUE)) {
    q <- matrixStats::colQuantiles(
      mat,
      probs = probs,
      na.rm = na.rm,
      type = type,
      digits = 7L,
      useNames = TRUE,
      drop = TRUE
    )
    return(t(q))
  }

  apply(mat, 2, quantile, probs = probs, na.rm = na.rm, type = type)
}

fast_prepare_quantile_data <- function(v_d, probs = c(0.975, 0.5, 0.025), type = 7L, na.rm = FALSE) {
  dims <- dim(v_d)
  if (length(dims) != 3) {
    stop("fast_prepare_quantile_data expects a 3D array")
  }

  d1 <- dims[1]
  d2 <- dims[2]
  d3 <- dims[3]

  mat <- matrix(v_d, nrow = d1 * d2, ncol = d3)

  if (requireNamespace("matrixStats", quietly = TRUE)) {
    q_mat <- matrixStats::rowQuantiles(
      mat,
      probs = probs,
      na.rm = na.rm,
      type = type,
      digits = 7L,
      useNames = FALSE,
      drop = TRUE
    )
  } else {
    q_mat <- t(apply(mat, 1, quantile, probs = probs, na.rm = na.rm, type = type, names = FALSE))
  }

  array(q_mat, dim = c(d1, d2, length(probs)))
}

# Fast alternative to tidyr::pivot_longer() for wide matrices/data frames.
# Produces row-major output (id row 1 with all columns, then row 2, etc),
# matching pivot_longer() behavior when pivoting columns for each row.
fast_long_by_row <- function(mat, row_values, col_values, row_name, col_name, value_name = "Value") {
  mat <- as.matrix(mat)

  if (length(row_values) != nrow(mat)) {
    stop("fast_long_by_row: row_values length must match nrow(mat)")
  }
  if (length(col_values) != ncol(mat)) {
    stop("fast_long_by_row: col_values length must match ncol(mat)")
  }

  out <- data.frame(
    row = rep(row_values, each = ncol(mat)),
    col = rep(col_values, times = nrow(mat)),
    value = as.vector(t(mat)),
    stringsAsFactors = FALSE
  )
  names(out) <- c(row_name, col_name, value_name)
  out
}

fast_long_ensembles <- function(ensemble_mat, dates, member_col = "member", value_name = "value") {
  mat <- as.matrix(ensemble_mat)
  members <- colnames(mat)
  if (is.null(members)) {
    members <- paste0("X", seq_len(ncol(mat)))
  }
  fast_long_by_row(
    mat = mat,
    row_values = dates,
    col_values = members,
    row_name = "Date",
    col_name = member_col,
    value_name = value_name
  )
}
