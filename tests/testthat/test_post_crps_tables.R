source(testthat::test_path("..", "..", "R", "environmetrics", "02_helpers_core.R"))

test_that("shared daily date helpers are available for smoke-fast CRPS exports", {
  mat <- matrix(1, nrow = 3, ncol = 2)
  expect_equal(
    daily_dates_for_matrix_rows(mat, start_date = as.Date("2022-12-26"), context = "ut.rows"),
    as.Date("2022-12-26") + 0:2
  )
  expect_equal(
    daily_dates_for_matrix_cols(mat, start_date = as.Date("2022-12-26"), context = "ut.cols"),
    as.Date("2022-12-26") + 0:1
  )
})

test_that("post_crps_quantile_approx returns zero for perfect forecast samples", {
  sample_mat <- matrix(2, nrow = 7, ncol = 5)
  obs <- rep(2, 5)

  out <- post_crps_quantile_approx(obs = obs, sample_mat = sample_mat, context = "ut.crps.zero")

  expect_equal(length(out$crps), 5L)
  expect_equal(out$crps, rep(0, 5))
  expect_equal(out$n_samples_eff, rep(7L, 5L))
  expect_equal(out$n_samples_nominal, 7L)
  expect_equal(out$method, "quantile_check_loss_sum")
  expect_equal(out$tau_rule, "k_over_m_plus_1")
})

test_that("post_truth_from_start_or_na records missing and short truth without stopping", {
  missing <- expect_warning(
    post_truth_from_start_or_na(
      usgs_dates = as.Date("2022-12-20") + 0:2,
      usgs_truth = c(1, 2, 3),
      forecast_start_date = as.Date("2022-12-26"),
      horizon = 4,
      context = "ut.truth.missing"
    ),
    "TRUTH_MISSING"
  )
  expect_equal(missing$truth, rep(NA_real_, 4))
  expect_equal(missing$availability$status, "missing")
  expect_equal(missing$availability$truth_rows_available, 0L)
  expect_equal(missing$availability$horizon_days, 4L)

  short <- expect_warning(
    post_truth_from_start_or_na(
      usgs_dates = as.Date("2022-12-26") + 0:1,
      usgs_truth = c(10, 11),
      forecast_start_date = as.Date("2022-12-26"),
      horizon = 4,
      context = "ut.truth.short"
    ),
    "TRUTH_SHORT"
  )
  expect_equal(short$truth, c(10, 11, NA, NA))
  expect_equal(short$availability$status, "short")
  expect_equal(short$availability$truth_rows_available, 2L)
  expect_equal(short$availability$truth_rows_used, 2L)
})

test_that("post_crps_model_tables returns expected schemas and horizon-aligned rows", {
  set.seed(101)
  sample_mat <- matrix(rnorm(6 * 4, mean = 1.5, sd = 0.2), nrow = 6, ncol = 4)
  obs <- c(1.2, 1.4, 1.6, 1.8)
  dates <- as.Date("2022-12-26") + 0:3

  out <- post_crps_model_tables(
    model_id = "toy_model",
    model_family = "synthesis",
    model_variant = "toy_variant",
    sample_mat = sample_mat,
    obs = obs,
    forecast_dates = dates,
    cutoff_date = as.Date("2022-12-25"),
    forecast_start_date = as.Date("2022-12-26"),
    transfer_mode = "drop",
    score_scale = "log_cms_plus1",
    context = "ut.crps.tables"
  )

  expect_equal(nrow(out$per_time), 4L)
  expect_equal(nrow(out$summary), 1L)
  expect_equal(out$summary$horizon_days[[1L]], 4L)
  expect_equal(out$summary$model_id[[1L]], "toy_model")
  expect_true(all(out$per_time$model_id == "toy_model"))
  expect_true(all(out$per_time$lead_day == 1:4))
  expect_true(all(out$per_time$forecast_date == as.character(dates)))
  expect_equal(
    names(out$per_time),
    c(
      "cutoff_date", "forecast_start_date", "model_id", "model_family", "model_variant",
      "transfer_mode", "lead_day", "forecast_date", "crps", "n_samples_eff",
      "n_samples_nominal", "score_method", "tau_rule", "score_scale"
    )
  )
})

test_that("post_export_crps_tables writes suffixed table files and manifest", {
  td <- tempfile("crps_export_tables_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  per_time <- data.frame(
    cutoff_date = "2022-12-25",
    forecast_start_date = "2022-12-26",
    model_id = "toy_model",
    model_family = "synthesis",
    model_variant = "toy_variant",
    transfer_mode = "keep",
    lead_day = 1:2,
    forecast_date = as.character(as.Date("2022-12-26") + 0:1),
    crps = c(0.1, 0.2),
    n_samples_eff = c(10L, 10L),
    n_samples_nominal = c(10L, 10L),
    score_method = "quantile_check_loss_sum",
    tau_rule = "k_over_m_plus_1",
    score_scale = "log_cms_plus1",
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    cutoff_date = "2022-12-25",
    forecast_start_date = "2022-12-26",
    model_id = "toy_model",
    model_family = "synthesis",
    model_variant = "toy_variant",
    transfer_mode = "keep",
    horizon_days = 2L,
    n_valid = 2L,
    mean_crps = 0.15,
    median_crps = 0.15,
    sd_crps = 0.07071068,
    min_crps = 0.1,
    max_crps = 0.2,
    n_samples_nominal = 10L,
    n_samples_eff_min = 10L,
    n_samples_eff_max = 10L,
    score_method = "quantile_check_loss_sum",
    tau_rule = "k_over_m_plus_1",
    score_scale = "log_cms_plus1",
    stringsAsFactors = FALSE
  )

  out <- post_export_crps_tables(
    per_time_df = per_time,
    summary_df = summary,
    output_dir = td,
    table_formats = c("csv", "rds"),
    keep_na = TRUE,
    numeric_digits = 17L,
    file_suffix = "_keep"
  )

  expect_true(file.exists(file.path(td, "crps_forecast_per_time_keep.csv")))
  expect_true(file.exists(file.path(td, "crps_forecast_summary_keep.csv")))
  expect_true(file.exists(file.path(td, "crps_forecast_per_time_keep.rds")))
  expect_true(file.exists(file.path(td, "crps_forecast_summary_keep.rds")))
  expect_equal(
    names(out$manifest),
    c("table_name", "file_path", "nrow", "ncol", "sha256")
  )
  expect_equal(nrow(out$manifest), 4L)
  expect_true(all(nzchar(out$manifest$sha256)))
})

test_that("post_crps_synth_model_meta resolves IDs for exAL and AL families", {
  u_ex <- post_crps_synth_model_meta(family = "univar", likelihood_mode = "exal")
  u_al <- post_crps_synth_model_meta(family = "univar", likelihood_mode = "al")
  m_ex_keep <- post_crps_synth_model_meta(family = "multivar", likelihood_mode = "exal", transfer_mode = "keep")
  m_al_drop <- post_crps_synth_model_meta(family = "multivar", likelihood_mode = "al", transfer_mode = "drop")
  n_keep <- post_crps_synth_model_meta(family = "ndlm", transfer_mode = "keep")
  nu_keep <- post_crps_synth_model_meta(family = "ndlm_univar", transfer_mode = "keep")

  expect_equal(u_ex$model_id, "exdqlm_univar_synth")
  expect_equal(u_al$model_id, "dqlm_univar_al_synth")
  expect_equal(m_ex_keep$model_id, "exdqlm_multivar_synth_keep")
  expect_equal(m_al_drop$model_id, "dqlm_multivar_al_synth_drop")
  expect_equal(n_keep$model_id, "ndlm_main_synth_keep")
  expect_equal(n_keep$model_variant, "ndlm_main_keep")
  expect_equal(nu_keep$model_id, "ndlm_univar_synth_keep")
  expect_equal(nu_keep$model_variant, "ndlm_univar_keep")
})

test_that("post lightweight helper names caches and sample subsets deterministically", {
  expect_equal(
    post_cache_file_name(
      "synth_multivar_forecast_log1p.rds",
      model_id = "dqlm_multivar_al_synth_keep",
      transfer_mode = "keep"
    ),
    "dqlm_multivar_al_synth_keep__mode-keep__synth_multivar_forecast_log1p.rds"
  )
  expect_equal(
    post_cache_file_name("plain.rds", model_id = "", transfer_mode = NA_character_),
    "plain.rds"
  )

  expect_equal(post_plot_sample_indices(5, cap = 10), 1:5)
  expect_equal(post_plot_sample_indices(0, cap = 10), integer(0))
  expect_equal(post_plot_sample_indices(10, cap = 4), c(1L, 4L, 7L, 10L))
})

test_that("post_crps_input_health_tables reports nonfinite draw health failures", {
  sample_mat <- matrix(
    c(1, 2, 3, NaN, 5, 6, Inf, 8),
    nrow = 4,
    ncol = 2
  )
  out <- post_crps_input_health_tables(
    model_id = "toy_health",
    model_family = "synthesis",
    model_variant = "toy_variant",
    sample_mat = sample_mat,
    forecast_dates = as.Date("2022-12-26") + 0:1,
    cutoff_date = as.Date("2022-12-25"),
    forecast_start_date = as.Date("2022-12-26"),
    transfer_mode = "keep",
    min_finite_share = 1,
    max_abs = NA_real_,
    context = "ut.crps.health"
  )

  expect_false(out$pass)
  expect_equal(nrow(out$summary), 1L)
  expect_equal(nrow(out$per_time), 2L)
  expect_equal(out$summary$status[[1L]], "fail")
  expect_true(out$summary$n_nonfinite_cells[[1L]] > 0L)
  expect_true(all(out$per_time$n_nonfinite >= 0L))
})

test_that("post_transform_loglog1p_array can cap overflow-risk latent draws", {
  limit <- safe_exp_limit()
  latent <- array(
    c(0, 1, limit + 1, limit + 25),
    dim = c(2, 2, 1)
  )

  expect_error(
    post_transform_loglog1p_array(latent, context = "ut.overflow", overflow_policy = "error"),
    "EXP_OVERFLOW_RISK"
  )

  out <- suppressWarnings(
    post_transform_loglog1p_array(latent, context = "ut.overflow", overflow_policy = "cap")
  )
  expect_true(all(is.finite(out$values)))
  expect_equal(out$summary$n_overflow_risk, 2L)
  expect_equal(out$summary$n_capped, 2L)
  expect_equal(
    out$values[1, 2, 1],
    exp(limit),
    tolerance = 1e-12
  )
  expect_equal(
    out$values[2, 2, 1],
    exp(limit),
    tolerance = 1e-12
  )
})

test_that("post_export_crps_input_health_tables writes expected files", {
  td <- tempfile("crps_input_health_export_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  summary <- data.frame(
    cutoff_date = "2022-12-25",
    forecast_start_date = "2022-12-26",
    model_id = "toy_health",
    model_family = "synthesis",
    model_variant = "toy_variant",
    transfer_mode = "keep",
    horizon_days = 2L,
    n_samples_nominal = 4L,
    n_total_cells = 8L,
    n_finite_cells = 6L,
    n_nonfinite_cells = 2L,
    finite_share_cells = 0.75,
    n_horizon_with_nonfinite = 2L,
    min_finite_share_threshold = 1,
    min_finite_share_observed = 0.5,
    max_abs_threshold = NA_real_,
    max_abs_observed = 8,
    min_draw = 1,
    q01_draw = 1.05,
    median_draw = 4,
    q99_draw = 7.95,
    max_draw = 8,
    mean_draw = 4.1666667,
    sd_draw = 2.6394444,
    status = "fail",
    violations = "nonfinite_cells=2",
    stringsAsFactors = FALSE
  )
  per_time <- data.frame(
    cutoff_date = "2022-12-25",
    forecast_start_date = "2022-12-26",
    model_id = "toy_health",
    model_family = "synthesis",
    model_variant = "toy_variant",
    transfer_mode = "keep",
    lead_day = 1:2,
    forecast_date = as.character(as.Date("2022-12-26") + 0:1),
    n_samples_nominal = 4L,
    n_finite = c(3L, 3L),
    n_nonfinite = c(1L, 1L),
    finite_share = c(0.75, 0.75),
    min_draw = c(1, 2),
    q01_draw = c(1.02, 2.02),
    median_draw = c(2, 5),
    q99_draw = c(2.98, 7.98),
    max_draw = c(3, 8),
    mean_draw = c(2, 5),
    sd_draw = c(1, 1.7320508),
    max_abs_draw = c(3, 8),
    stringsAsFactors = FALSE
  )

  out <- post_export_crps_input_health_tables(
    summary_df = summary,
    per_time_df = per_time,
    output_dir = td,
    table_formats = c("csv", "rds"),
    keep_na = TRUE,
    numeric_digits = 17L,
    file_suffix = "_keep"
  )

  expect_true(file.exists(file.path(td, "crps_input_health_keep.csv")))
  expect_true(file.exists(file.path(td, "crps_input_health_per_time_keep.csv")))
  expect_true(file.exists(file.path(td, "crps_input_health_keep.rds")))
  expect_true(file.exists(file.path(td, "crps_input_health_per_time_keep.rds")))
  expect_equal(
    names(out$manifest),
    c("table_name", "file_path", "nrow", "ncol", "sha256")
  )
  expect_equal(nrow(out$manifest), 4L)
})
