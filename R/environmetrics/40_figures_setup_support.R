###############################################################################
# Setup/support figure module for cutoff-specific exAL-M-T1 input bundles
# Inputs:
#   - Objects created by 00_paths.R / 10_data_inputs.R from run-scoped inputs
#   - OUT_DIR: destination directory for rendered figures
# Outputs:
#   - usgs.png
#   - precip_soilmoisture_climatePC1_faceted_labeled.png
#   - retrospective_log_discharge_plot_faceted.png
#   - forecats.png
###############################################################################

if (!exists("OUT_DIR", inherits = TRUE) || !nzchar(get("OUT_DIR", inherits = TRUE))) {
  stop("40_figures_setup_support.R requires OUT_DIR to be defined.", call. = FALSE)
}

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

format_date_long <- function(x) {
  format(as.Date(x), "%B %d, %Y")
}

safe_as_date <- function(x) {
  out <- suppressWarnings(as.Date(x))
  if (length(out) == 0L || is.na(out[[1L]])) as.Date(NA) else out[[1L]]
}

special_event_date <- safe_as_date(Sys.getenv("UNIFIED_FORECAST_EVENT_DATE", ""))
special_event_label <- Sys.getenv("UNIFIED_FORECAST_EVENT_LABEL", "")

cutoff_label_short <- format(as.Date(CUTOFF_DATE), "%b %d")

daily_dates_for_n <- function(start_date, n_days, context = "dates") {
  start_date <- as.Date(start_date)
  n_use <- as.integer(n_days[[1]])
  if (length(start_date) != 1L || is.na(start_date)) {
    stop(sprintf("[%s] start_date must be a valid scalar Date.", context), call. = FALSE)
  }
  if (!is.finite(n_use) || n_use <= 0L) {
    stop(sprintf("[%s] n_days must be a positive finite integer.", context), call. = FALSE)
  }
  seq(start_date, by = "1 day", length.out = n_use)
}

daily_dates_for_matrix_rows <- function(mat, start_date, context = "dates.rows") {
  if (is.null(dim(mat)) || length(dim(mat)) != 2L) {
    stop(sprintf("[%s] expected a 2D object for row-based date construction.", context), call. = FALSE)
  }
  daily_dates_for_n(start_date = start_date, n_days = nrow(mat), context = context)
}

read_kv_map <- function(path) {
  out <- list()
  path <- as.character(path)
  if (!length(path) || is.na(path[[1]]) || !nzchar(path[[1]]) || !file.exists(path[[1]])) return(out)
  lines <- readLines(path[[1]], warn = FALSE)
  if (!length(lines)) return(out)
  for (ln in lines) {
    pos <- regexpr("=", ln, fixed = TRUE)
    if (pos[[1]] <= 1L) next
    key <- trimws(substr(ln, 1L, pos[[1]] - 1L))
    val <- trimws(substr(ln, pos[[1]] + 1L, nchar(ln)))
    if (nzchar(key)) out[[key]] <- val
  }
  out
}

choose_source_by_priority <- function(df, source_regex, priorities) {
  rows <- df[grepl(source_regex, df$source_id), c("Date", "source_id", "discharge"), drop = FALSE]
  if (nrow(rows) == 0L) return(data.frame(Date = as.Date(character(0)), value = numeric(0)))
  rows$priority <- match(rows$source_id, priorities)
  rows$priority[is.na(rows$priority)] <- length(priorities) + 1L
  rows <- rows[order(rows$Date, rows$priority, rows$source_id), , drop = FALSE]
  rows <- rows[!duplicated(rows$Date), c("Date", "discharge"), drop = FALSE]
  names(rows)[2] <- "value"
  rows
}

resolve_retros_selection_policy <- function(bundle_root, cutoff_date) {
  glofas_priority <- c(
    "glofas_hist_v40_lisflood_cons",
    "glofas_hist_v31_lisflood_cons",
    "glofas_hist_v21_htessel_cons",
    "glofas_legacy_reanalysis_v30",
    "glofas_synth_retro_ens_mean"
  )
  nws_priority <- c(
    "nws_synth_retro_ens_mean",
    "nws_retro_v30",
    "nws_retro_v21",
    "nws_retro_v20",
    "nws_retro_v12"
  )
  if (!nzchar(bundle_root)) return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
  meta_path <- file.path(bundle_root, "meta.yaml")
  if (!file.exists(meta_path)) return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
  meta <- tryCatch(yaml::read_yaml(meta_path), error = function(e) NULL)
  sel <- meta$config$inputs$retros$selection_policy
  if (!is.list(sel)) return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))

  `%or_default%` <- function(x, y) if (is.null(x)) y else x
  cutoff_use <- suppressWarnings(as.Date(cutoff_date))

  pick_window_source <- function(windows, cutoff_date) {
    if (!is.list(windows) || is.na(cutoff_date)) return("")
    for (w in windows) {
      if (!is.list(w)) next
      src <- tolower(as.character(w$source_id %or_default% ""))
      if (!nzchar(src)) next
      start <- suppressWarnings(as.Date(as.character(w$start %or_default% NA_character_)))
      end <- suppressWarnings(as.Date(as.character(w$end %or_default% NA_character_)))
      if (is.na(start) || is.na(end)) next
      if (cutoff_date >= start && cutoff_date <= end) return(src)
    }
    ""
  }

  keep_ids <- tolower(as.character(unlist(sel$keep_source_ids %or_default% character(0), use.names = FALSE)))
  keep_ids <- keep_ids[nzchar(keep_ids)]
  keep_glofas <- keep_ids[grepl("glofas", keep_ids)]
  keep_nws <- keep_ids[grepl("^nws", keep_ids)]
  if (length(keep_glofas) > 0L) glofas_priority <- unique(c(keep_glofas, glofas_priority))
  if (length(keep_nws) > 0L) nws_priority <- unique(c(keep_nws, nws_priority))

  win_glofas <- pick_window_source(sel$glofas_by_cutoff_windows, cutoff_use)
  win_nws <- pick_window_source(sel$nws_by_cutoff_windows, cutoff_use)
  if (nzchar(win_glofas)) glofas_priority <- unique(c(win_glofas, glofas_priority))
  if (nzchar(win_nws)) nws_priority <- unique(c(win_nws, nws_priority))

  list(glofas_priority = glofas_priority, nws_priority = nws_priority)
}

build_forecats_retros_plot <- function() {
  fallback <- data.frame(
    Date = as.Date(timestamps),
    GloFAS = as.numeric(Y[2, ]),
    NWS = as.numeric(Y[3, ]),
    stringsAsFactors = FALSE
  )

  if (!exists("RETROS_PATH", inherits = TRUE)) return(fallback)
  retros_path <- as.character(get("RETROS_PATH", inherits = TRUE))
  if (!nzchar(retros_path) || !file.exists(retros_path)) return(fallback)

  retros_wide <- tryCatch(read.csv(retros_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (!is.data.frame(retros_wide) || nrow(retros_wide) == 0L) return(fallback)
  date_col <- if ("Date" %in% names(retros_wide)) "Date" else if ("date" %in% names(retros_wide)) "date" else ""
  if (!nzchar(date_col)) return(fallback)
  ncol_name <- if ("NWS3.0" %in% names(retros_wide)) "NWS3.0" else if ("NWS" %in% names(retros_wide)) "NWS" else ""
  if (!('USGS' %in% names(retros_wide)) || !('GloFAS' %in% names(retros_wide)) || !nzchar(ncol_name)) return(fallback)

  retros_wide <- data.frame(
    Date = as.Date(retros_wide[[date_col]]),
    USGS = suppressWarnings(as.numeric(retros_wide$USGS)),
    GloFAS = suppressWarnings(as.numeric(retros_wide$GloFAS)),
    NWS = suppressWarnings(as.numeric(retros_wide[[ncol_name]])),
    stringsAsFactors = FALSE
  )
  retros_wide <- retros_wide[!is.na(retros_wide$Date), , drop = FALSE]

  shared_root <- dirname(dirname(retros_path))
  source_map <- read_kv_map(file.path(shared_root, "source_map.txt"))
  snapshot_root <- as.character(source_map[["snapshot_root"]])
  if (!length(snapshot_root) || is.na(snapshot_root[[1]]) || !nzchar(snapshot_root[[1]])) snapshot_root <- "" else snapshot_root <- snapshot_root[[1]]
  snap_map <- read_kv_map(file.path(snapshot_root, "snapshot_source_map.txt"))
  bundle_root <- as.character(snap_map[["bundle_root"]])
  if (!length(bundle_root) || is.na(bundle_root[[1]]) || !nzchar(bundle_root[[1]])) bundle_root <- "" else bundle_root <- bundle_root[[1]]
  long_path <- file.path(bundle_root, "inputs", "retros_daily.csv")
  if (nzchar(bundle_root) && file.exists(long_path)) {
    long_retro <- tryCatch(read.csv(long_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.data.frame(long_retro) && ('source_id' %in% names(long_retro)) && ('discharge_cms' %in% names(long_retro))) {
      dcol <- if ("date" %in% names(long_retro)) "date" else if ("Date" %in% names(long_retro)) "Date" else ""
      if (nzchar(dcol)) {
        long_tbl <- data.frame(
          Date = as.Date(long_retro[[dcol]]),
          source_id = tolower(as.character(long_retro$source_id)),
          discharge = suppressWarnings(as.numeric(long_retro$discharge_cms)),
          stringsAsFactors = FALSE
        )
        long_tbl <- long_tbl[!is.na(long_tbl$Date) & is.finite(long_tbl$discharge), , drop = FALSE]
        if (is.finite(CUTOFF_DATE)) long_tbl <- long_tbl[long_tbl$Date <= CUTOFF_DATE, , drop = FALSE]
        policy <- resolve_retros_selection_policy(bundle_root, CUTOFF_DATE)
        glofas_sel <- choose_source_by_priority(long_tbl, "glofas", policy$glofas_priority)
        nws_sel <- choose_source_by_priority(long_tbl, "^nws", policy$nws_priority)
        if (nrow(glofas_sel) > 0L && nrow(nws_sel) > 0L) {
          names(glofas_sel)[2] <- "GloFAS"
          names(nws_sel)[2] <- "NWS"
          retros_wide <- merge(retros_wide[, c("Date", "USGS"), drop = FALSE], glofas_sel, by = "Date", all = FALSE)
          retros_wide <- merge(retros_wide, nws_sel, by = "Date", all = FALSE)
        }
      }
    }
  }

  usgs_ref <- data.frame(
    Date = as.Date(San_Lorenzo_Daily_USGS_R$time),
    usgs_raw = suppressWarnings(as.numeric(San_Lorenzo_Daily_USGS_R$X_00060_00003) * CFSToCMS_CONVERSION_FACTOR),
    stringsAsFactors = FALSE
  )
  cmp <- merge(retros_wide[, c("Date", "USGS"), drop = FALSE], usgs_ref, by = "Date", all = FALSE)
  cmp <- cmp[is.finite(cmp$USGS) & is.finite(cmp$usgs_raw), , drop = FALSE]
  scale_mode <- "log1p_cms"
  if (nrow(cmp) >= 10L) {
    mae_raw <- mean(abs(cmp$USGS - cmp$usgs_raw), na.rm = TRUE)
    mae_log1p <- mean(abs(expm1(cmp$USGS) - cmp$usgs_raw), na.rm = TRUE)
    if (is.finite(mae_raw) && is.finite(mae_log1p) && mae_raw <= mae_log1p) scale_mode <- "raw_cms"
  }

  to_loglog <- function(x, scale_mode) {
    x <- suppressWarnings(as.numeric(x))
    x[!is.finite(x) | x <= 0] <- NA_real_
    if (identical(scale_mode, "raw_cms")) return(log(log(x + 1)))
    log(x)
  }

  out <- data.frame(
    Date = as.Date(retros_wide$Date),
    GloFAS = to_loglog(retros_wide$GloFAS, scale_mode),
    NWS = to_loglog(retros_wide$NWS, scale_mode),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$GloFAS) & is.finite(out$NWS), , drop = FALSE]
  if (nrow(out) < 10L) return(fallback)
  out
}

compute_special_event_label_y <- function(values, offset = 0.15, fallback = -0.15) {
  y <- suppressWarnings(min(values, na.rm = TRUE))
  if (!is.finite(y)) return(fallback)
  y - offset
}

safe_max_date <- function(values, fallback_date) {
  fallback_date <- as.Date(fallback_date)
  out <- suppressWarnings(max(as.Date(values), na.rm = TRUE))
  if (!is.finite(out)) return(fallback_date)
  as.Date(out, origin = "1970-01-01")
}

add_special_event_marker <- function(plot_obj, label_y) {
  if (is.na(special_event_date) || !nzchar(special_event_label)) return(plot_obj)
  plot_obj +
    geom_vline(
      xintercept = as.numeric(special_event_date),
      color = "#4a235a",
      linetype = "dashed",
      linewidth = 0.5,
      alpha = 0.8
    ) +
    annotate(
      "text",
      x = special_event_date,
      y = label_y,
      label = special_event_label,
      color = "#4a235a",
      vjust = 4,
      hjust = -0.1,
      fontface = "bold",
      size = 3.5
    )
}

render_setup_support_figures <- function(output_dir) {
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  flood_stages_ft <- c(21.76, 16.5)^3
  flood_stages_cm <- flood_stages_ft * CFSToCMS_CONVERSION_FACTOR
  flood_stages_trans <- log(log(flood_stages_cm + 1))
  flood_stage_labels <- c("Major Flooding", "Minor Flooding")

  event_dates <- as.Date(c("1998-02-03", "2004-07-01", "2017-02-21", "2023-01-09"))
  event_numbers <- as.character(1:4)
  event_color <- "#D95F02"

  flow_data <- data.frame(Date = as.Date(timestamps), Flow = Y[1, ])
  flow_end_date <- suppressWarnings(max(as.Date(flow_data$Date), na.rm = TRUE))
  flow_start_date <- suppressWarnings(min(as.Date(flow_data$Date), na.rm = TRUE))
  label_y <- max(flow_data$Flow, na.rm = TRUE) + 0.1 * diff(range(flow_data$Flow, na.rm = TRUE))

  p_usgs <- ggplot(flow_data, aes(x = Date, y = Flow)) +
    geom_line(color = "#238b45", linewidth = 0.7, alpha = 0.92) +
    geom_vline(xintercept = event_dates, color = event_color, linetype = "dashed", linewidth = 0.5) +
    annotate("text", x = event_dates, y = rep(label_y, length(event_dates)), label = event_numbers, fontface = "bold", color = event_color, size = 4, vjust = 0, hjust = 2) +
    geom_hline(yintercept = flood_stages_trans, linetype = c("dashed", "dashed"), color = c("gray", "gray"), linewidth = 0.8) +
    annotate("text", x = max(flow_data$Date), y = flood_stages_trans, label = flood_stage_labels, hjust = 10.5, vjust = -0.3, color = c("black", "black"), fontface = "italic", size = 3.5) +
    labs(
      title = "Daily Flow of San Lorenzo River at Big Trees, CA",
      subtitle = sprintf("Measurements from %s to %s", format_date_long(flow_start_date), format_date_long(flow_end_date)),
      x = "Year",
      y = expression("Water Flow (Log-Log cm^3/s)")
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 13, face = "italic", hjust = 0.5), axis.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  ggsave(file.path(output_dir, "usgs.png"), plot = p_usgs, width = 12, height = 6, units = "in", dpi = 900)

  series_colors <- c("Precipitation" = "#1b9e77", "Soil_Moisture" = "#386cb0", "Climate_PC1" = "#e6550d")
  df_covariates <- data.frame(Date = as.Date(timestamps), Precipitation = X[, 1], Soil_Moisture = X[, 2], GDPC1 = X[, 3])
  df_plot <- df_covariates
  colnames(df_plot) <- c("Date", "Precipitation", "Soil_Moisture", "Climate_PC1")
  df_long <- fast_long_by_row(mat = df_plot[, c("Precipitation", "Soil_Moisture", "Climate_PC1")], row_values = df_plot$Date, col_values = c("Precipitation", "Soil_Moisture", "Climate_PC1"), row_name = "Date", col_name = "Variable", value_name = "Value")
  df_long$Variable <- factor(df_long$Variable, levels = c("Precipitation", "Soil_Moisture", "Climate_PC1"))
  custom_labels <- c(Precipitation = "Precipitation", Soil_Moisture = "Soil Moisture", Climate_PC1 = "1st Principal Comp.")
  p_cov <- ggplot(df_long, aes(x = Date, y = Value, color = Variable)) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_color_manual(values = series_colors) +
    facet_wrap(~Variable, ncol = 1, scales = "free_y", strip.position = "left", labeller = as_labeller(custom_labels)) +
    labs(title = "Exogeneous Data (Scaled)", subtitle = sprintf("Historical covariates through %s", format_date_long(CUTOFF_DATE)), x = "Year", y = NULL) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 13, face = "italic", hjust = 0.5), axis.title.x = element_text(face = "bold"), axis.text = element_text(size = 12), strip.text = element_text(face = "bold", size = 13, color = "black"), strip.background = element_blank(), legend.position = "none", panel.grid.minor = element_blank())
  ggsave(file.path(output_dir, "precip_soilmoisture_climatePC1_faceted_labeled.png"), plot = p_cov, width = 12, height = 8, units = "in", dpi = 900)

  df_retro <- build_forecats_retros_plot()
  p_glofas <- ggplot(df_retro, aes(x = Date, y = GloFAS)) +
    geom_line(color = "#E67E22", linewidth = 0.7, alpha = 0.92) +
    labs(title = "GloFAS Retrospective Analysis", x = NULL, y = expression("Water Flow (Log-Log cm^3/s)")) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ylim(-2, 2) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), axis.text = element_text(size = 12), panel.grid.minor = element_blank())
  p_nws <- ggplot(df_retro, aes(x = Date, y = NWS)) +
    geom_line(color = "#756bb1", linewidth = 0.7, alpha = 0.92) +
    labs(title = "NWS Retrospective Analysis", x = "Year", y = expression("Water Flow (Log-Log cm^3/s)")) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ylim(-3, 2) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"), axis.text = element_text(size = 12), panel.grid.minor = element_blank())
  p_retro <- p_glofas / p_nws + patchwork::plot_layout(ncol = 1)
  ggsave(file.path(output_dir, "retrospective_log_discharge_plot_faceted.png"), plot = p_retro, width = 12, height = 8, units = "in", dpi = 900)

  plot_start <- PLOT_START_DATE
  plot_end <- PLOT_END_DATE
  df_retro_long <- fast_long_by_row(mat = df_retro[, c("GloFAS", "NWS")], row_values = df_retro$Date, col_values = c("GloFAS", "NWS"), row_name = "Date", col_name = "Source", value_name = "Value")
  df_retro_long$Source <- factor(df_retro_long$Source, levels = c("GloFAS", "NWS"))
  df_retro_plot <- dplyr::filter(df_retro_long, Date >= plot_start & Date < FORECAST_START_DATE)
  usgs_plot_df <- San_Lorenzo_Daily_USGS_R %>%
    dplyr::filter(time >= plot_start & time <= plot_end) %>%
    dplyr::mutate(obs_type = ifelse(time >= FORECAST_START_DATE, "After", "Before"), value = log(log(X_00060_00003 * CFSToCMS_CONVERSION_FACTOR + 1))) %>%
    dplyr::filter(is.finite(value))
  special_label_y <- compute_special_event_label_y(usgs_plot_df$value, offset = 0.15, fallback = -0.15)
  usgs_right_x <- safe_max_date(usgs_plot_df$time, fallback_date = plot_end)
  forecast_start <- FORECAST_START_DATE
  glofas_dates <- daily_dates_for_matrix_rows(ensembles[[1]], start_date = forecast_start, context = "ensemble_dates.glofas")
  nws_dates <- daily_dates_for_matrix_rows(ensembles[[2]], start_date = forecast_start, context = "ensemble_dates.nws")
  glofas_color <- "#E67E22"
  nws_color <- "#756bb1"
  usgs_green <- "#238b45"
  usgs_after_color <- "#B22222"
  usgs_before_df <- usgs_plot_df %>% dplyr::filter(obs_type == "Before") %>% dplyr::mutate(Source = "USGS")
  usgs_after_df <- usgs_plot_df %>% dplyr::filter(obs_type == "After") %>% dplyr::mutate(Source = "USGS")
  glofas_before_df <- df_retro_plot %>% dplyr::filter(Source == "GloFAS")
  nws_before_df <- df_retro_plot %>% dplyr::filter(Source == "NWS")

  p_forecasts <- ggplot() +
    annotate("text", x = usgs_right_x, y = flood_stages_trans, label = flood_stage_labels, hjust = 10.5, vjust = -0.5, color = c("black", "black"), fontface = "italic", size = 3.5) +
    annotate("text", x = CUTOFF_DATE, y = special_label_y, label = cutoff_label_short, color = "gray40", size = 3.5, fontface = "bold", vjust = 4, hjust = -0.1) +
    geom_hline(yintercept = flood_stages_trans, linetype = c("dashed", "dashed"), color = c("gray", "gray"), linewidth = 0.8) +
    geom_line(data = usgs_before_df, aes(x = time, y = value, color = Source, linetype = Source), linewidth = 0.5) +
    geom_point(data = usgs_before_df, aes(x = time, y = value, color = Source, shape = Source), size = 1.4) +
    geom_line(data = glofas_before_df, aes(x = Date, y = Value, color = Source, linetype = Source), linewidth = 0.5, alpha = 0.85) +
    geom_point(data = glofas_before_df, aes(x = Date, y = Value, color = Source, shape = Source), size = 1.4, alpha = 0.85) +
    geom_line(data = nws_before_df, aes(x = Date, y = Value, color = Source, linetype = Source), linewidth = 0.5, alpha = 0.85) +
    geom_point(data = nws_before_df, aes(x = Date, y = Value, color = Source, shape = Source), size = 1.4, alpha = 0.85) +
    geom_line(data = fast_long_ensembles(ensembles[[1]], glofas_dates), aes(x = Date, y = value, group = member), color = glofas_color, alpha = 0.22, linewidth = 0.5, show.legend = FALSE) +
    geom_line(data = fast_long_ensembles(ensembles[[2]], nws_dates), aes(x = Date, y = value, group = member), color = nws_color, alpha = 0.22, linewidth = 0.5, show.legend = FALSE) +
    geom_line(data = usgs_after_df, aes(x = time, y = value), color = usgs_after_color, linewidth = 0.5, linetype = "dashed", show.legend = FALSE) +
    geom_point(data = usgs_after_df, aes(x = time, y = value), color = usgs_after_color, size = 2, show.legend = FALSE) +
    scale_x_date(breaks = scales::pretty_breaks(6), date_labels = "%b %d") +
    scale_color_manual(name = "Data Source", values = c("USGS" = usgs_green, "GloFAS" = glofas_color, "NWS" = nws_color)) +
    scale_linetype_manual(name = "Data Source", values = c("USGS" = "solid", "GloFAS" = "solid", "NWS" = "solid")) +
    scale_shape_manual(name = "Data Source", values = c("USGS" = 16, "GloFAS" = 16, "NWS" = 16)) +
    geom_vline(xintercept = as.numeric(CUTOFF_DATE), color = "gray40", linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
    labs(title = "Observed and Retrospective River Flow with GloFAS and NWS Forecast Ensembles", subtitle = sprintf("Cutoff %s; forecast starts %s", format_date_long(CUTOFF_DATE), format_date_long(FORECAST_START_DATE)), x = "Date", y = expression("Water Flow (Log-Log cm^3/s)")) +
    guides(color = guide_legend(override.aes = list(size = 2)), shape = guide_legend(override.aes = list(size = 3))) +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5, margin = margin(b = 8)), axis.title = element_text(face = "bold"), legend.position = "top", legend.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  p_forecasts <- add_special_event_marker(p_forecasts, special_label_y)
  ggsave(file.path(output_dir, "forecats.png"), plot = p_forecasts, width = 12, height = 6, units = "in", dpi = 900)
}

render_setup_support_figures(OUT_DIR)
