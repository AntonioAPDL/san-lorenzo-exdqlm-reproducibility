source(testthat::test_path("..", "..", "R", "environmetrics", "02_helpers_core.R"))

mock_gamma_sigma <- function() {
  quantiles <- c("95th", "50th", "05th", "20th", "35th", "65th", "80th")
  sources <- c("NWS", "GLOFAS", "USGS")
  vars <- c("Gamma", "Sigma")

  grid <- expand.grid(
    variable = vars,
    source = sources,
    quantile = quantiles,
    stringsAsFactors = FALSE
  )

  idx <- seq_len(nrow(grid))
  grid$quantile_025 <- idx / 100
  grid$median <- idx / 50
  grid$quantile_975 <- idx / 25
  grid
}

test_that("post_export_gamma_sigma_tables writes deterministic schema and ordering", {
  td <- tempfile("posterior_tables_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  mock <- mock_gamma_sigma()

  out <- post_export_gamma_sigma_tables(
    all_quantiles = mock,
    output_dir = td,
    ci_digits = 5L,
    write_tex = TRUE,
    table_formats = c("csv", "rds")
  )

  expect_true(file.exists(file.path(td, "gamma_summary.csv")))
  expect_true(file.exists(file.path(td, "sigma_summary.csv")))
  expect_true(file.exists(file.path(td, "gamma_summary.rds")))
  expect_true(file.exists(file.path(td, "sigma_summary.rds")))
  expect_true(file.exists(file.path(td, "gamma_summary.tex")))
  expect_true(file.exists(file.path(td, "sigma_summary.tex")))

  expect_equal(names(out$gamma), c("quantile", "source", "stat", "center", "q2_5", "q97_5", "ci_str"))
  expect_equal(names(out$sigma), c("quantile", "source", "stat", "center", "q2_5", "q97_5", "ci_str"))
  expect_equal(nrow(out$gamma), 21L)
  expect_equal(nrow(out$sigma), 21L)

  expect_equal(sort(unique(out$gamma$quantile)), c(5L, 20L, 35L, 50L, 65L, 80L, 95L))
  expect_equal(sort(unique(out$sigma$quantile)), c(5L, 20L, 35L, 50L, 65L, 80L, 95L))
  expect_equal(unique(out$gamma$source), c("USGS", "GLOFAS", "NWS"))
  expect_equal(unique(out$sigma$source), c("USGS", "GLOFAS", "NWS"))
  expect_equal(
    out$gamma$center[out$gamma$quantile == 5L & out$gamma$source == "USGS"],
    mock$median[mock$variable == "Gamma" & mock$source == "USGS" & mock$quantile == "05th"]
  )
  expect_equal(
    out$sigma$center[out$sigma$quantile == 95L & out$sigma$source == "NWS"],
    mock$median[mock$variable == "Sigma" & mock$source == "NWS" & mock$quantile == "95th"]
  )

  expect_true(all(nzchar(out$gamma$ci_str)))
  expect_true(all(nzchar(out$sigma$ci_str)))
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{5}, -?[0-9]+\\.[0-9]{5}$", out$gamma$ci_str)))
  gamma_tex <- readLines(file.path(td, "gamma_summary.tex"), warn = FALSE)
  expect_true(any(grepl("[0-9]+\\.[0-9]{5}", gamma_tex)))
  expect_false(any(grepl("[0-9]+\\.[0-9]{6}", gamma_tex)))
  expect_equal(
    names(out$manifest),
    c("table_name", "file_path", "nrow", "ncol", "sha256")
  )
  expect_equal(nrow(out$manifest), 4L)
  expect_true(all(nzchar(out$manifest$sha256)))
})

test_that("post_export_gamma_sigma_tables handles empty input safely", {
  td <- tempfile("posterior_tables_empty_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  empty <- data.frame(
    variable = character(0),
    source = character(0),
    quantile = character(0),
    quantile_025 = numeric(0),
    median = numeric(0),
    quantile_975 = numeric(0),
    stringsAsFactors = FALSE
  )

  out <- post_export_gamma_sigma_tables(
    all_quantiles = empty,
    output_dir = td,
    write_tex = FALSE
  )

  expect_equal(nrow(out$gamma), 0L)
  expect_equal(nrow(out$sigma), 0L)
  expect_true(file.exists(file.path(td, "gamma_summary.csv")))
  expect_true(file.exists(file.path(td, "sigma_summary.csv")))
})

test_that("post_export_covariate_effects_table writes expected columns and stable order", {
  td <- tempfile("covariate_tables_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  summary_df <- data.frame(
    Component = c(25, 23, 24, 23, 24, 25),
    Quantile = c("95th", "50th", "5th", "95th", "50th", "5th"),
    Lower = c(-0.03, 2.35, 0.11, 2.91, 0.113, -0.087),
    Mean = c(-0.02, 2.39, 0.115, 2.95, 0.118, -0.084),
    Upper = c(-0.01, 2.43, 0.119, 2.98, 0.124, -0.080),
    stringsAsFactors = FALSE
  )

  out <- post_export_covariate_effects_table(
    summary_df = summary_df,
    output_dir = td,
    time_index = 999L,
    ci_digits = 5L,
    write_tex = TRUE,
    table_formats = c("csv")
  )

  expect_true(file.exists(file.path(td, "covariate_effects_summary.csv")))
  expect_true(file.exists(file.path(td, "covariate_effects_summary.tex")))

  expect_equal(
    names(out$table),
    c("covariate", "quantile", "center", "q2_5", "q97_5", "ci_str", "time_index", "notes")
  )
  expect_equal(unique(out$table$covariate), c("Precipitation", "Soil Moisture", "PC1"))
  expect_true(all(out$table$time_index == 999L))
  expect_equal(
    out$table$center[out$table$covariate == "Precipitation" & out$table$quantile == 50L],
    summary_df$Mean[summary_df$Component == 23L & summary_df$Quantile == "50th"]
  )
  expect_equal(
    out$table$center[out$table$covariate == "PC1" & out$table$quantile == 95L],
    summary_df$Mean[summary_df$Component == 25L & summary_df$Quantile == "95th"]
  )
  expect_true(all(nzchar(out$table$ci_str)))
  expect_true(all(grepl("^-?[0-9]+\\.[0-9]{5}, -?[0-9]+\\.[0-9]{5}$", out$table$ci_str)))
  cov_tex <- readLines(file.path(td, "covariate_effects_summary.tex"), warn = FALSE)
  expect_true(any(grepl("[0-9]+\\.[0-9]{5}", cov_tex)))
  expect_false(any(grepl("[0-9]+\\.[0-9]{6}", cov_tex)))
  expect_equal(nrow(out$manifest), 1L)
  expect_true(nzchar(out$manifest$sha256[[1L]]))
})

test_that("posterior table export README records mixed center policy", {
  td <- tempfile("posterior_table_readme_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  post_write_table_exports_readme(td)
  readme <- readLines(file.path(td, "posterior_table_exports_README.md"), warn = FALSE)

  expect_true(any(grepl("gamma_summary\\.csv: gamma by source x quantile with center=posterior median", readme)))
  expect_true(any(grepl("sigma_summary\\.csv: sigma by source x quantile with center=posterior median", readme)))
  expect_true(any(grepl("covariate_effects_summary\\.csv: transfer-function covariate effects with center=posterior mean", readme)))
})

test_that("post_export_tables csv bytes are deterministic after stable ordering", {
  td <- tempfile("deterministic_csv_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  a <- data.frame(
    id = c(3, 1, 2),
    score = c(1.2, 5.4, 2.2),
    label = c("z", "x", "y"),
    stringsAsFactors = FALSE
  )
  b <- a[c(2, 3, 1), , drop = FALSE]

  m1 <- post_export_tables(
    tables = list(example = a),
    output_dir = file.path(td, "one"),
    formats = "csv",
    sort_keys = list(example = c("id")),
    keep_na = TRUE
  )
  m2 <- post_export_tables(
    tables = list(example = b),
    output_dir = file.path(td, "two"),
    formats = "csv",
    sort_keys = list(example = c("id")),
    keep_na = TRUE
  )

  p1 <- file.path(file.path(td, "one"), m1$file_path[[1L]])
  p2 <- file.path(file.path(td, "two"), m2$file_path[[1L]])
  bytes1 <- readBin(p1, what = "raw", n = file.info(p1)$size)
  bytes2 <- readBin(p2, what = "raw", n = file.info(p2)$size)
  expect_identical(bytes1, bytes2)
  expect_identical(m1$sha256[[1L]], m2$sha256[[1L]])
  expect_false(startsWith(m1$file_path[[1L]], "/"))
  expect_false(startsWith(m2$file_path[[1L]], "/"))
})

test_that("post_export_tables preserves row order when sort_keys is NULL", {
  td <- tempfile("preserve_order_csv_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  a <- data.frame(
    id = c(3, 1, 2),
    score = c(1.2, 5.4, 2.2),
    label = c("z", "x", "y"),
    stringsAsFactors = FALSE
  )
  b <- a[c(2, 3, 1), , drop = FALSE]

  m1 <- post_export_tables(
    tables = list(example = a),
    output_dir = file.path(td, "one"),
    formats = "csv",
    keep_na = TRUE
  )
  m2 <- post_export_tables(
    tables = list(example = b),
    output_dir = file.path(td, "two"),
    formats = "csv",
    keep_na = TRUE
  )

  p1 <- file.path(file.path(td, "one"), m1$file_path[[1L]])
  p2 <- file.path(file.path(td, "two"), m2$file_path[[1L]])
  bytes1 <- readBin(p1, what = "raw", n = file.info(p1)$size)
  bytes2 <- readBin(p2, what = "raw", n = file.info(p2)$size)
  expect_false(identical(bytes1, bytes2))
})

test_that("post_export_tables keep_na policy is explicit and stable", {
  td <- tempfile("na_policy_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  x <- data.frame(
    id = c(1L, 2L, 3L),
    value = c(10.0, NA_real_, 20.5),
    stringsAsFactors = FALSE
  )

  keep <- post_export_tables(
    tables = list(tbl = x),
    output_dir = file.path(td, "keep"),
    formats = "csv",
    keep_na = TRUE,
    sort_keys = list(tbl = "id")
  )
  drop <- post_export_tables(
    tables = list(tbl = x),
    output_dir = file.path(td, "drop"),
    formats = "csv",
    keep_na = FALSE,
    sort_keys = list(tbl = "id")
  )

  keep_df <- read.csv(file.path(file.path(td, "keep"), keep$file_path[[1L]]), stringsAsFactors = FALSE)
  drop_df <- read.csv(file.path(file.path(td, "drop"), drop$file_path[[1L]]), stringsAsFactors = FALSE)
  expect_equal(nrow(keep_df), 3L)
  expect_equal(nrow(drop_df), 2L)
  expect_false(any(is.na(drop_df$value)))
})

test_that("post_write_table_exports_manifest writes stable schema with checksum", {
  td <- tempfile("manifest_export_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  m <- post_export_tables(
    tables = list(
      t1 = data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)
    ),
    output_dir = td,
    formats = c("csv", "rds"),
    keep_na = TRUE
  )
  out_path <- post_write_table_exports_manifest(m, output_dir = td)
  expect_true(file.exists(out_path))
  m_df <- read.csv(out_path, stringsAsFactors = FALSE)
  expect_equal(
    names(m_df),
    c("table_name", "file_path", "nrow", "ncol", "sha256")
  )
  expect_true(all(nzchar(m_df$sha256)))
  expect_equal(m_df$file_path[[1L]], "t1.csv")
})

test_that("post_path_relative_to_dir derives relative path and falls back to basename", {
  td <- tempfile("relative_path_helper_")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)

  in_dir_missing <- file.path(td, "nested", "table.csv")
  expect_equal(post_path_relative_to_dir(in_dir_missing, td), file.path("nested", "table.csv"))

  outside_missing <- file.path(tempdir(), "outside_path_helper", "table_outside.csv")
  expect_equal(post_path_relative_to_dir(outside_missing, td), "table_outside.csv")
})
