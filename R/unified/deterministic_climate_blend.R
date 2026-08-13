# unified/deterministic_climate_blend.R
#
# Shared helpers for deterministic-climate ensemble reduction, noise injection,
# and historical observed/forecast blending. These helpers are used by both the
# live covariate materialization path and the forecast-review plotting workflow
# so that the plotted blended series matches the series that would be fed into
# the rerun models.

`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

detclim_get_nested <- function(x, keys, default = NULL) {
  cur <- x
  for (key in keys) {
    if (!is.list(cur) || is.null(cur[[key]])) {
      return(default)
    }
    cur <- cur[[key]]
  }
  cur
}

detclim_parse_reduction_spec <- function(reduction) {
  text <- tolower(trimws(as.character(reduction %||% "mean")[[1L]]))
  if (text %in% c("mean", "median", "max")) {
    return(list(method = text, quantile = NULL, label = text))
  }
  if (grepl("^[qp][0-9]+(\\.[0-9]+)?$", text)) {
    numeric_text <- sub("^[qp]", "", text)
    qval <- suppressWarnings(as.numeric(numeric_text))
    if (is.finite(qval)) {
      if (qval > 1) qval <- qval / 100
      if (qval >= 0 && qval <= 1) {
        return(list(method = "quantile", quantile = qval, label = text))
      }
    }
  }
  stop(sprintf("Unsupported deterministic_climate reduction: %s", text), call. = FALSE)
}

detclim_reduce_values <- function(values, reduction = "mean") {
  spec <- detclim_parse_reduction_spec(reduction)
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(NA_real_)
  }
  if (identical(spec$method, "mean")) {
    return(mean(values, na.rm = TRUE))
  }
  if (identical(spec$method, "median")) {
    return(stats::median(values, na.rm = TRUE))
  }
  if (identical(spec$method, "max")) {
    return(max(values, na.rm = TRUE))
  }
  stats::quantile(values, probs = spec$quantile, na.rm = TRUE, type = 7)[[1L]]
}

detclim_derive_seed <- function(base_seed, label) {
  seed <- suppressWarnings(as.integer(base_seed))
  if (!is.finite(seed)) {
    stop(sprintf("deterministic_climate noise_seed must be an integer, got: %s", as.character(base_seed)), call. = FALSE)
  }
  chars <- utf8ToInt(as.character(label %||% "detclim"))
  mod <- 2147483646
  hash <- 0
  if (length(chars) > 0L) {
    for (i in seq_along(chars)) {
      hash <- ((hash * 131) + as.numeric(chars[[i]]) + as.numeric(i)) %% mod
    }
  }
  derived <- (as.numeric(seed) + hash) %% mod
  if (!is.finite(derived) || derived <= 0) derived <- as.numeric(seed)
  as.integer(derived)
}

detclim_apply_noise <- function(values, noise_sd = 0, noise_seed = 1L, floor_at_zero = FALSE, label = "detclim", noise_distribution = "normal") {
  values <- as.numeric(values)
  if (length(values) == 0L) {
    return(list(value = values, noise = numeric(0), seed = detclim_derive_seed(noise_seed, label)))
  }
  sd_val <- suppressWarnings(as.numeric(noise_sd %||% 0))
  if (!is.finite(sd_val) || sd_val < 0) {
    stop(sprintf("deterministic_climate noise_sd must be numeric >= 0, got: %s", as.character(noise_sd)), call. = FALSE)
  }
  noise_distribution <- tolower(trimws(as.character(noise_distribution %||% "normal")[[1L]]))
  if (!(noise_distribution %in% c("normal", "abs_normal"))) {
    stop(
      sprintf(
        "deterministic_climate noise_distribution must be one of: normal, abs_normal; got: %s",
        noise_distribution
      ),
      call. = FALSE
    )
  }
  derived_seed <- detclim_derive_seed(noise_seed, label)
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(derived_seed)
  noise <- if (sd_val > 0) stats::rnorm(length(values), mean = 0, sd = sd_val) else rep(0, length(values))
  if (identical(noise_distribution, "abs_normal")) {
    noise <- abs(noise)
  }
  out <- values + noise
  if (isTRUE(floor_at_zero)) {
    out <- pmax(0, out)
  }
  list(value = out, noise = noise, seed = derived_seed)
}

detclim_apply_zero_stay_gate <- function(values, observed_values, zero_stay_prob = NULL, zero_stay_seed = 1L, label = "detclim") {
  values <- as.numeric(values)
  observed_values <- as.numeric(observed_values)
  if (length(values) == 0L) {
    return(list(
      value = values,
      draw = numeric(0),
      applied = logical(0),
      seed = detclim_derive_seed(zero_stay_seed, paste(label, "zero_stay", sep = "|"))
    ))
  }
  prob <- suppressWarnings(as.numeric(zero_stay_prob))
  if (!is.finite(prob) || prob < 0 || prob > 1) {
    stop(
      sprintf(
        "deterministic_climate observed_zero_stay_prob must be numeric in [0, 1], got: %s",
        as.character(zero_stay_prob)
      ),
      call. = FALSE
    )
  }
  derived_seed <- detclim_derive_seed(zero_stay_seed, paste(label, "zero_stay", sep = "|"))
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(derived_seed)
  draw <- stats::runif(length(values))
  applied <- is.finite(observed_values) & observed_values <= 0 & draw < prob
  out <- ifelse(applied, 0, values)
  list(value = out, draw = draw, applied = applied, seed = derived_seed)
}

detclim_compose_future_series <- function(
  observed_df,
  forecast_df,
  observed_weight = 0.9,
  noise_sd = 0,
  noise_seed = 1L,
  floor_at_zero = FALSE,
  noise_distribution = "normal",
  observed_zero_stay_prob = NULL,
  observed_zero_stay_seed = NULL,
  label = "detclim"
) {
  observed_weight <- suppressWarnings(as.numeric(observed_weight))
  if (!is.finite(observed_weight) || observed_weight < 0 || observed_weight > 1) {
    stop(sprintf("deterministic_climate observed_weight must be numeric in [0, 1], got: %s", as.character(observed_weight)), call. = FALSE)
  }
  observed_df <- observed_df[, c("date", "value"), drop = FALSE]
  forecast_df <- forecast_df[, c("date", "value"), drop = FALSE]
  names(observed_df)[[2L]] <- "observed_value"
  names(forecast_df)[[2L]] <- "forecast_value"
  merged <- merge(observed_df, forecast_df, by = "date", all = FALSE, sort = TRUE)
  if (nrow(merged) == 0L) {
    stop(sprintf("deterministic_climate could not align observed and forecast future rows for %s", label), call. = FALSE)
  }
  noisy <- detclim_apply_noise(
    values = merged$forecast_value,
    noise_sd = noise_sd,
    noise_seed = noise_seed,
    floor_at_zero = floor_at_zero,
    label = label,
    noise_distribution = noise_distribution
  )
  merged$noise <- noisy$noise
  merged$noisy_forecast_value <- noisy$value
  merged$observed_weight <- observed_weight
  merged$forecast_weight <- 1 - observed_weight
  merged$blended_value <- observed_weight * merged$observed_value + (1 - observed_weight) * merged$noisy_forecast_value
  merged$noise_seed_effective <- noisy$seed
  merged$noise_distribution <- noise_distribution
  zero_prob <- suppressWarnings(as.numeric(observed_zero_stay_prob))
  if (length(zero_prob) < 1L || !is.finite(zero_prob[[1L]])) {
    zero_prob <- NA_real_
  } else {
    zero_prob <- zero_prob[[1L]]
  }
  if (is.finite(zero_prob) && zero_prob > 0) {
    zero_gate <- detclim_apply_zero_stay_gate(
      values = merged$blended_value,
      observed_values = merged$observed_value,
      zero_stay_prob = zero_prob,
      zero_stay_seed = observed_zero_stay_seed %||% noise_seed,
      label = label
    )
    merged$observed_zero_stay_prob <- zero_prob
    merged$observed_zero_stay_draw <- zero_gate$draw
    merged$observed_zero_stay_applied <- zero_gate$applied
    merged$observed_zero_stay_seed_effective <- zero_gate$seed
    merged$blended_value_effective <- zero_gate$value
  } else {
    merged$observed_zero_stay_prob <- NA_real_
    merged$observed_zero_stay_draw <- NA_real_
    merged$observed_zero_stay_applied <- FALSE
    merged$observed_zero_stay_seed_effective <- NA_integer_
    merged$blended_value_effective <- merged$blended_value
  }
  merged
}

detclim_normalize_series_cfg <- function(det_cfg, series_name) {
  defaults <- list(
    enabled = TRUE,
    source = if (identical(series_name, "precip")) "gefs_apcp" else "nwm_soilsat_top",
    reduction = "mean",
    dry_day_threshold_mm = if (identical(series_name, "precip")) 0 else NULL,
    noisy_blend = list(
      enabled = FALSE,
      noise_sd = 0,
      noise_seed = 20260415L,
      noise_distribution = "normal",
      floor_at_zero = identical(series_name, "precip")
    ),
    observed_blend = list(
      enabled = FALSE,
      observed_weight = 0.9,
      observed_zero_stay_prob = NULL,
      observed_zero_stay_seed = 20260415L
    )
  )
  series_cfg <- detclim_get_nested(det_cfg, list(series_name), default = list())
  if (!is.list(series_cfg)) {
    series_cfg <- list()
  }
  out <- utils::modifyList(defaults, series_cfg, keep.null = TRUE)
  out$enabled <- isTRUE(out$enabled)
  out$source <- tolower(trimws(as.character(out$source %||% defaults$source)[[1L]]))
  out$reduction <- tolower(trimws(as.character(out$reduction %||% defaults$reduction)[[1L]]))
  out$noisy_blend$enabled <- isTRUE(detclim_get_nested(out, list("noisy_blend", "enabled"), default = FALSE))
  out$noisy_blend$noise_sd <- suppressWarnings(as.numeric(detclim_get_nested(out, list("noisy_blend", "noise_sd"), default = defaults$noisy_blend$noise_sd)))
  out$noisy_blend$noise_seed <- suppressWarnings(as.integer(detclim_get_nested(out, list("noisy_blend", "noise_seed"), default = defaults$noisy_blend$noise_seed)))
  out$noisy_blend$noise_distribution <- tolower(trimws(as.character(detclim_get_nested(out, list("noisy_blend", "noise_distribution"), default = defaults$noisy_blend$noise_distribution))[[1L]]))
  out$noisy_blend$floor_at_zero <- isTRUE(detclim_get_nested(out, list("noisy_blend", "floor_at_zero"), default = defaults$noisy_blend$floor_at_zero))
  out$observed_blend$enabled <- isTRUE(detclim_get_nested(out, list("observed_blend", "enabled"), default = FALSE))
  out$observed_blend$observed_weight <- suppressWarnings(as.numeric(detclim_get_nested(out, list("observed_blend", "observed_weight"), default = defaults$observed_blend$observed_weight)))
  prob_raw <- detclim_get_nested(out, list("observed_blend", "observed_zero_stay_prob"), default = defaults$observed_blend$observed_zero_stay_prob)
  out$observed_blend$observed_zero_stay_prob <- if (is.null(prob_raw) || length(prob_raw) < 1L) NA_real_ else suppressWarnings(as.numeric(prob_raw[[1L]]))
  out$observed_blend$observed_zero_stay_seed <- suppressWarnings(as.integer(detclim_get_nested(out, list("observed_blend", "observed_zero_stay_seed"), default = defaults$observed_blend$observed_zero_stay_seed)))
  out
}
