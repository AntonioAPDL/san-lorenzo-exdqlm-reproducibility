source(testthat::test_path("..", "..", "R", "environmetrics", "02_helpers_core.R"))
source(testthat::test_path("..", "..", "R", "environmetrics", "utils_data.R"))

testthat::test_that("post synthesis helper repairs crossing quantile sample cubes", {
  testthat::skip_if_not_installed("exdqlm")

  q_probs <- c(0.05, 0.50, 0.95)
  n_samp <- 25L
  horizon <- 4L
  base <- seq(-0.2, 0.8, length.out = n_samp)
  sample_cube <- array(NA_real_, dim = c(length(q_probs), n_samp, horizon))
  for (t_idx in seq_len(horizon)) {
    sample_cube[1L, , t_idx] <- base + 3 + 0.05 * t_idx
    sample_cube[2L, , t_idx] <- base + 0 + 0.05 * t_idx
    sample_cube[3L, , t_idx] <- base + 1 + 0.05 * t_idx
  }

  raw_crossing <- post_quantile_crossing_summary(
    sample_cube = sample_cube,
    q_probs = q_probs,
    context = "ut.raw"
  )
  testthat::expect_equal(raw_crossing$summary$mean_crossing_rate[[1L]], 1)

  out <- post_synthesize_rearranged_sample_cube(
    sample_cube = sample_cube,
    q_probs = q_probs,
    n_samp = n_samp,
    seed = 20260522L,
    enforce_isotonic = TRUE,
    rearrange = TRUE,
    grid_M = 101L,
    sort_draws_by_time = TRUE,
    context = "ut.synthesis"
  )

  testthat::expect_s3_class(out, "post_quantile_synthesis")
  testthat::expect_equal(dim(out$sample_mat), c(n_samp, horizon))
  testthat::expect_equal(dim(out$quantiles), c(length(q_probs), horizon))
  testthat::expect_equal(dim(out$anchor_quantiles), c(length(q_probs), horizon))
  testthat::expect_true(all(apply(out$sample_mat, 2L, function(x) all(diff(x) >= 0))))
  testthat::expect_true(all(apply(out$anchor_quantiles, 2L, function(x) all(diff(x) >= -1e-12))))
  testthat::expect_true(all(apply(out$quantiles, 2L, function(x) all(diff(x) >= -1e-12))))
  testthat::expect_equal(out$diagnostics$summary$raw_sample_mean_crossing_rate[[1L]], 1)
  testthat::expect_equal(out$diagnostics$summary$anchor_curve_crossing_share[[1L]], 0)
  testthat::expect_equal(out$diagnostics$summary$empirical_curve_crossing_share[[1L]], 0)
})

testthat::test_that("post synthesis helper is deterministic under a fixed seed", {
  testthat::skip_if_not_installed("exdqlm")

  q_probs <- c(0.20, 0.50, 0.80)
  sample_cube <- array(NA_real_, dim = c(3L, 15L, 3L))
  for (t_idx in seq_len(dim(sample_cube)[3])) {
    sample_cube[1L, , t_idx] <- seq(0, 1, length.out = 15L) + t_idx
    sample_cube[2L, , t_idx] <- seq(1, 2, length.out = 15L) + t_idx
    sample_cube[3L, , t_idx] <- seq(2, 3, length.out = 15L) + t_idx
  }

  out1 <- post_synthesize_rearranged_sample_cube(
    sample_cube = sample_cube,
    q_probs = q_probs,
    n_samp = 15L,
    seed = 99L,
    grid_M = 51L,
    context = "ut.det1"
  )
  out2 <- post_synthesize_rearranged_sample_cube(
    sample_cube = sample_cube,
    q_probs = q_probs,
    n_samp = 15L,
    seed = 99L,
    grid_M = 51L,
    context = "ut.det2"
  )

  testthat::expect_equal(out1$sample_mat, out2$sample_mat)
  testthat::expect_equal(out1$quantiles, out2$quantiles)
})

testthat::test_that("quantile synthesis cache names include the synthesis method tag", {
  method_tag <- post_quantile_synthesis_method_tag(
    enforce_isotonic = TRUE,
    rearrange = TRUE,
    grid_M = 1001L,
    method = "exdqlm"
  )
  testthat::expect_match(method_tag, "exdqlm_iso1_rearr1_grid1001_v1", fixed = TRUE)

  cache_name <- post_quantile_synthesis_cache_file_name(
    base_name = "synth_multivar_forecast_log1p.rds",
    method_tag = method_tag,
    model_id = "exdqlm_multivar_synth_keep",
    transfer_mode = "keep"
  )
  testthat::expect_match(cache_name, "exdqlm_multivar_synth_keep__mode-keep__", fixed = TRUE)
  testthat::expect_match(cache_name, method_tag, fixed = TRUE)
  testthat::expect_match(cache_name, "\\.rds$")
})

testthat::test_that("formal synthesis helper rejects a single quantile lane", {
  sample_cube <- array(0, dim = c(1L, 4L, 2L))
  testthat::expect_error(
    post_synthesize_rearranged_sample_cube(
      sample_cube = sample_cube,
      q_probs = 0.20,
      n_samp = 4L,
      context = "ut.single_q"
    ),
    "ut.single_q_Q_PROBS",
    fixed = TRUE
  )
})

testthat::test_that("active smoke-fast multivariate path uses formal rearranged synthesis", {
  smoke_path <- testthat::test_path("..", "..", "R", "environmetrics", "40_figures_smoke_fast.R")
  smoke_text <- readLines(smoke_path, warn = FALSE)
  testthat::expect_true(any(grepl("post_synthesize_rearranged_sample_cube", smoke_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("smoke_multivar_synthesis_method_tag", smoke_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("synth_multivar_forecast_diagnostics.rds", smoke_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("synth_multivar_hist_diagnostics.rds", smoke_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("smoke_multivar_can_synthesize_quantile_grid", smoke_text, fixed = TRUE)))
  testthat::expect_true(any(grepl("SKIP_SINGLE_Q", smoke_text, fixed = TRUE)))
})
