###############################################################################
# exdqlm_multivar_structure.R
#
# Shared structural contract for the legacy multivariate exDQLM bridge.
# This keeps fit/post aligned when toggling trend and seasonal harmonics.
###############################################################################

exdqlm_multivar_default_harmonics <- function() {
  c(1, 2, 1 / 6.8068493)
}

exdqlm_multivar_combine_mods <- function(mod1, mod2) {
  if (inherits(mod1, "exdqlm") && inherits(mod2, "exdqlm")) {
    if (exists("combineMods", envir = asNamespace("exdqlm"), inherits = FALSE)) {
      return(get("combineMods", envir = asNamespace("exdqlm"), inherits = FALSE)(mod1, mod2))
    }
    if (exists("combineMods", mode = "function", inherits = TRUE)) {
      return(get("combineMods", mode = "function", inherits = TRUE)(mod1, mod2))
    }
    return(mod1 + mod2)
  }
  if (inherits(mod1, "dlm") && inherits(mod2, "dlm")) {
    return(get("%+%", envir = asNamespace("dlm"))(mod1, mod2))
  }
  stop("exdqlm_multivar_combine_mods requires matching 'exdqlm' or 'dlm' objects.", call. = FALSE)
}

exdqlm_multivar_normalize_flag <- function(raw, default = TRUE) {
  if (is.null(raw) || length(raw) == 0L) {
    return(isTRUE(default))
  }
  val <- raw[[1L]]
  if (is.logical(val)) {
    return(isTRUE(val))
  }
  if (is.numeric(val)) {
    return(is.finite(val) && val != 0)
  }
  txt <- tolower(trimws(as.character(val)))
  if (!nzchar(txt)) {
    return(isTRUE(default))
  }
  if (txt %in% c("1", "true", "yes", "y", "on")) {
    return(TRUE)
  }
  if (txt %in% c("0", "false", "no", "n", "off")) {
    return(FALSE)
  }
  isTRUE(default)
}

exdqlm_multivar_normalize_harmonic_indices <- function(
  raw,
  n_total = length(exdqlm_multivar_default_harmonics()),
  default = seq_len(n_total)
) {
  if (is.null(raw) || length(raw) == 0L) {
    return(as.integer(default))
  }

  vals <- raw
  if (length(vals) == 1L && is.character(vals)) {
    txt <- trimws(vals[[1L]])
    if (!nzchar(txt) || tolower(txt) %in% c("all", "default")) {
      return(as.integer(default))
    }
    vals <- unlist(strsplit(txt, ",", fixed = TRUE), use.names = FALSE)
  }

  vals <- suppressWarnings(as.integer(trimws(as.character(vals))))
  vals <- vals[is.finite(vals)]
  vals <- unique(vals)
  vals <- vals[vals >= 1L & vals <= n_total]
  if (length(vals) == 0L) {
    return(as.integer(integer(0)))
  }
  sort(as.integer(vals))
}

exdqlm_multivar_encode_harmonic_indices <- function(indices) {
  vals <- exdqlm_multivar_normalize_harmonic_indices(indices)
  if (length(vals) == 0L) {
    return("")
  }
  paste(vals, collapse = ",")
}

exdqlm_multivar_read_structure_spec <- function(
  include_trend_raw = NULL,
  enabled_harmonic_indices_raw = NULL,
  default_harmonics = exdqlm_multivar_default_harmonics()
) {
  include_trend <- exdqlm_multivar_normalize_flag(include_trend_raw, default = TRUE)
  enabled_indices <- exdqlm_multivar_normalize_harmonic_indices(
    enabled_harmonic_indices_raw,
    n_total = length(default_harmonics),
    default = seq_along(default_harmonics)
  )

  if (!isTRUE(include_trend) && length(enabled_indices) == 0L) {
    stop(
      "exdqlm multivar structure cannot disable both trend and all seasonal harmonics.",
      call. = FALSE
    )
  }

  list(
    include_trend = include_trend,
    enabled_harmonic_indices = enabled_indices,
    enabled_harmonics = default_harmonics[enabled_indices],
    disabled_harmonic_indices = setdiff(seq_along(default_harmonics), enabled_indices),
    default_harmonics = default_harmonics
  )
}

exdqlm_multivar_first_nonempty_env <- function(keys, default = "") {
  for (key in keys) {
    raw <- Sys.getenv(key, "")
    if (nzchar(raw)) {
      return(raw)
    }
  }
  default
}

exdqlm_multivar_read_structure_spec_from_env <- function(
  include_trend_keys = c("DISC_W_INCLUDE_TREND", "UNIFIED_EXDQLM_MULTIVAR_INCLUDE_TREND"),
  enabled_harmonic_keys = c("DISC_W_ENABLED_HARMONIC_INDICES", "UNIFIED_EXDQLM_MULTIVAR_ENABLED_HARMONIC_INDICES"),
  default_harmonics = exdqlm_multivar_default_harmonics()
) {
  include_trend_raw <- exdqlm_multivar_first_nonempty_env(include_trend_keys, default = "")
  harmonic_raw <- exdqlm_multivar_first_nonempty_env(enabled_harmonic_keys, default = "")
  exdqlm_multivar_read_structure_spec(
    include_trend_raw = include_trend_raw,
    enabled_harmonic_indices_raw = harmonic_raw,
    default_harmonics = default_harmonics
  )
}

exdqlm_multivar_build_structure <- function(
  m_yy,
  kk,
  df_t,
  df_s1,
  df_s2,
  df_s67,
  lam1,
  lam2,
  include_trend = TRUE,
  enabled_harmonic_indices = seq_along(exdqlm_multivar_default_harmonics()),
  default_harmonics = exdqlm_multivar_default_harmonics(),
  season_period = 363.5854,
  trend_c0_scale = 1.0,
  season_c0_scale = 1.0
) {
  spec <- exdqlm_multivar_read_structure_spec(
    include_trend_raw = include_trend,
    enabled_harmonic_indices_raw = enabled_harmonic_indices,
    default_harmonics = default_harmonics
  )

  seasonal_dfs_all <- c(df_s1, df_s2, df_s67)
  seasonal_dfs <- seasonal_dfs_all[spec$enabled_harmonic_indices]

  components <- list()
  component_dims <- integer(0)
  base_df <- numeric(0)

  if (isTRUE(spec$include_trend)) {
    trend.comp <- polytrendMod(1, m0 = m_yy, C0 = trend_c0_scale * kk)
    components[[length(components) + 1L]] <- trend.comp
    component_dims <- c(component_dims, 1L)
    base_df <- c(base_df, df_t)
  }

  if (length(spec$enabled_harmonics) > 0L) {
    seas.comp <- seasMod(
      p = season_period,
      h = spec$enabled_harmonics,
      C0 = season_c0_scale * kk * diag(2 * length(spec$enabled_harmonics))
    )
    components[[length(components) + 1L]] <- seas.comp
    component_dims <- c(component_dims, rep(2L, length(spec$enabled_harmonics)))
    base_df <- c(base_df, seasonal_dfs)
  }

  if (length(components) == 0L) {
    stop("No exdqlm multivar structural components were enabled.", call. = FALSE)
  }

  model <- components[[1L]]
  if (length(components) > 1L) {
    for (ii in 2:length(components)) {
      model <- exdqlm_multivar_combine_mods(model, components[[ii]])
    }
  }

  df1 <- base_df
  df2 <- base_df
  if (isTRUE(spec$include_trend)) {
    df1[[1L]] <- df_t * lam1
    df2[[1L]] <- df_t * lam2
  }

  list(
    model = model,
    p = length(model$m0),
    df = as.numeric(base_df),
    dim.df = as.integer(component_dims),
    df1 = as.numeric(df1),
    df2 = as.numeric(df2),
    include_trend = isTRUE(spec$include_trend),
    enabled_harmonic_indices = as.integer(spec$enabled_harmonic_indices),
    enabled_harmonics = as.numeric(spec$enabled_harmonics),
    seasonal_discount_values = as.numeric(seasonal_dfs)
  )
}

exdqlm_multivar_create_block_diag <- function(A, n) {
  if (is.null(dim(A))) {
    A <- matrix(as.numeric(A), ncol = 1L)
  } else {
    A <- as.matrix(A)
  }
  if (!is.numeric(n) || n <= 0 || n != floor(n)) {
    stop("n must be a positive integer.")
  }
  block_diag_matrix <- bdiag(replicate(n, A, simplify = FALSE))
  as.matrix(block_diag_matrix)
}
