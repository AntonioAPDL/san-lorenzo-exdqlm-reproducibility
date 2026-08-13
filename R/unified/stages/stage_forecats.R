# unified/stages/stage_forecats.R

unified_stage_forecats <- function(cfg, run_root, repo_root, manifest) {
  fore_root <- file.path(run_root, "forecats")
  dir.create(fore_root, recursive = TRUE, showWarnings = FALSE)

  mode <- cfg$inputs$forecats$mode
  bundle_root <- NULL

  snapshot_cfg <- cfg$inputs$forecats$snapshot
  if (is.null(snapshot_cfg)) snapshot_cfg <- list()
  snapshot_enabled <- snapshot_cfg$enabled
  if (is.null(snapshot_enabled)) {
    snapshot_enabled <- identical(mode, "build")
  }
  snapshot_dest_rel <- snapshot_cfg$dest_rel
  if (is.null(snapshot_dest_rel) || !nzchar(snapshot_dest_rel)) {
    snapshot_dest_rel <- "inputs/shared/forecats_bundle"
  }
  snapshot_copy_list <- snapshot_cfg$copy_list
  if (is.null(snapshot_copy_list)) snapshot_copy_list <- list()
  snapshot_copy_list <- unlist(snapshot_copy_list, use.names = FALSE)

  synthesize_bundle_meta <- function(cfg, bundle_root) {
    cutoff_date <- suppressWarnings(as.Date(as.character(cfg$dates$cutoff_date)))
    plot_start <- suppressWarnings(as.Date(as.character(cfg$dates$plot_start)))
    plot_end <- suppressWarnings(as.Date(as.character(cfg$dates$plot_end)))
    forecast_start <- if (is.na(cutoff_date)) as.Date(NA) else cutoff_date + 1L

    list(
      run = list(
        run_id = basename(normalizePath(bundle_root, mustWork = FALSE)),
        created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        synthesized_by = "unified_stage_forecats"
      ),
      processing = list(
        storage_unit = "cms",
        bias_correction = FALSE,
        scale_correction = FALSE
      ),
      site = cfg$site,
      dates = list(
        cutoff_date = if (is.na(cutoff_date)) as.character(cfg$dates$cutoff_date) else format(cutoff_date, "%Y-%m-%d"),
        forecast_start_date = if (is.na(forecast_start)) NA_character_ else format(forecast_start, "%Y-%m-%d"),
        plot_start = if (is.na(plot_start)) as.character(cfg$dates$plot_start) else format(plot_start, "%Y-%m-%d"),
        plot_end = if (is.na(plot_end)) as.character(cfg$dates$plot_end) else format(plot_end, "%Y-%m-%d")
      ),
      paths = list(
        retros_daily = "inputs/retros_daily.csv",
        nws_weighted_daily = "inputs/nws_weighted_daily.csv",
        glofas_weighted_daily = "inputs/glofas_weighted_daily.csv",
        nws_members = "inputs/nws_members.csv",
        glofas_members = "inputs/glofas_members.csv"
      ),
      forecast_issue_policy = list(
        publication_protocol = "latest_forecast_only",
        cross_issue_weighting_used = FALSE,
        legacy_weighted_daily_filenames_are_aliases = TRUE,
        nws = list(
          selection_rule = "latest_issue_datetime_per_target_hour_member_then_daily_mean",
          cache_pattern = "forecast_cache/nws/cutoff_date=<cutoff>/nws_members.csv"
        ),
        glofas = list(
          selection_rule = "issue_date_equals_cutoff",
          cache_pattern = "forecast_cache/glofas/issue_date=<cutoff>/glofas_members.csv"
        )
      ),
      transforms = cfg$transforms,
      plot = cfg$plot,
      config = cfg
    )
  }

  resolve_bundle_artifact <- function(bundle_root, rel) {
    rel <- gsub("\\\\", "/", rel)
    candidates <- switch(
      rel,
      "meta.yaml" = c("meta.yaml"),
      "inputs/retros_daily.csv" = c(
        "inputs/retros_daily.csv",
        "inputs/retros.csv",
        "retros_daily.csv",
        "retros.csv",
        "retros_base.csv"
      ),
      "inputs/retrospective_preparation.csv" = c(
        "inputs/retrospective_preparation.csv",
        "retrospective_preparation.csv",
        "retros_policy_window.csv"
      ),
      "inputs/usgs_daily.csv" = c("inputs/usgs_daily.csv", "usgs_daily.csv"),
      "inputs/nws_weighted_daily.csv" = c(
        "inputs/nws_weighted_daily.csv",
        "inputs/nws_forecast.csv",
        "nws_weighted_daily.csv",
        "nws_forecast.csv"
      ),
      "inputs/glofas_weighted_daily.csv" = c(
        "inputs/glofas_weighted_daily.csv",
        "inputs/glofas_forecast.csv",
        "glofas_weighted_daily.csv",
        "glofas_forecast.csv"
      ),
      "inputs/nws_members.csv" = c(
        "inputs/nws_members.csv",
        "inputs/nws_members_daily.csv",
        "inputs/nws_forecast.csv",
        "nws_members.csv",
        "nws_forecast.csv"
      ),
      "inputs/glofas_members.csv" = c(
        "inputs/glofas_members.csv",
        "inputs/glofas_members_daily.csv",
        "inputs/glofas_members_forecast.csv",
        "inputs/glofas_forecast.csv",
        "glofas_members.csv",
        "glofas_forecast.csv"
      ),
      rel
    )
    paths <- normalizePath(file.path(bundle_root, candidates), mustWork = FALSE)
    paths <- paths[file.exists(paths)]
    if (length(paths) == 0L) return("")
    paths[[1L]]
  }

  pick_latest_file <- function(paths) {
    paths <- unique(normalizePath(paths[file.exists(paths)], mustWork = FALSE))
    if (length(paths) == 0L) return("")
    finfo <- file.info(paths)
    ord <- order(finfo$mtime, decreasing = TRUE, na.last = NA)
    if (length(ord) == 0L) return(paths[[1]])
    paths[[ord[[1]]]]
  }

  csv_has_finite_numeric <- function(path, min_rows = 1L, min_numeric_cols = 1L) {
    if (!file.exists(path)) return(FALSE)
    dat <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.data.frame(dat)) return(FALSE)
    if (nrow(dat) < min_rows) return(FALSE)
    num_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    if (length(num_cols) < min_numeric_cols) return(FALSE)
    vals <- as.matrix(dat[, num_cols, drop = FALSE])
    finite_mask <- is.finite(vals)
    if (!any(finite_mask, na.rm = TRUE)) return(FALSE)
    finite_rows <- rowSums(finite_mask, na.rm = TRUE) > 0L
    finite_cols <- colSums(finite_mask, na.rm = TRUE) > 0L
    if (sum(finite_rows, na.rm = TRUE) < min_rows) return(FALSE)
    if (sum(finite_cols, na.rm = TRUE) < min_numeric_cols) return(FALSE)
    TRUE
  }

  choose_snapshot_alias_source <- function(snapshot_root, candidates, label, min_rows = 1L, min_numeric_cols = 1L) {
    candidate_paths <- normalizePath(file.path(snapshot_root, candidates), mustWork = FALSE)
    existing <- candidate_paths[file.exists(candidate_paths)]
    if (length(existing) == 0L) {
      stop(
        sprintf("forecats snapshot alias selection for %s has no existing candidates: %s", label, paste(candidates, collapse = ", ")),
        call. = FALSE
      )
    }
    for (path in existing) {
      if (csv_has_finite_numeric(path, min_rows = min_rows, min_numeric_cols = min_numeric_cols)) {
        return(path)
      }
    }
    stop(
      sprintf(
        "forecats snapshot alias selection for %s found no finite numeric CSV candidates. Checked: %s",
        label,
        paste(existing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  parse_first_date_col <- function(df) {
    candidates <- c("Date", "date", "timestamp", "time", "target_date")
    for (nm in candidates) {
      if (nm %in% names(df)) {
        d <- suppressWarnings(as.Date(df[[nm]]))
        if (any(!is.na(d))) {
          return(list(name = nm, values = d))
        }
      }
    }
    NULL
  }

  coerce_numeric_col <- function(df, candidates) {
    for (nm in candidates) {
      if (nm %in% names(df)) {
        v <- suppressWarnings(as.numeric(df[[nm]]))
        if (any(is.finite(v))) return(v)
      }
    }
    rep(NA_real_, nrow(df))
  }

  drop_incomplete_retros <- function(df) {
    keep <- is.finite(df$USGS) & is.finite(df$GloFAS) & is.finite(df$NWS3.0)
    df[keep, c("Date", "USGS", "GloFAS", "NWS3.0"), drop = FALSE]
  }

  floor_nonpositive_retros <- function(df, floor_value = 1.0e-8) {
    cols <- c("USGS", "GloFAS", "NWS3.0")
    for (nm in cols) {
      vals <- suppressWarnings(as.numeric(df[[nm]]))
      bad <- is.finite(vals) & vals <= 0
      if (any(bad, na.rm = TRUE)) {
        vals[bad] <- floor_value
      }
      df[[nm]] <- vals
    }
    df
  }

  assert_retros_daily_continuity <- function(df, context_label = "retros_snapshot") {
    if (!is.data.frame(df) || nrow(df) < 2L) return(invisible(TRUE))
    d <- sort(unique(as.Date(df$Date)))
    if (length(d) < 2L) return(invisible(TRUE))
    g <- as.integer(diff(d))
    bad <- which(g > 1L)
    if (length(bad) == 0L) return(invisible(TRUE))
    max_gap <- max(g[bad], na.rm = TRUE)
    examples <- head(
      sprintf("%s->%s (%dd)", format(d[bad], "%Y-%m-%d"), format(d[bad + 1L], "%Y-%m-%d"), g[bad]),
      5L
    )
    stop(
      sprintf(
        paste0(
          "%s has non-daily date continuity gaps (%d gaps; max=%d days). ",
          "Examples: %s"
        ),
        context_label,
        length(bad),
        max_gap,
        paste(examples, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  build_snapshot_retros <- function(snapshot_root, bundle_root, cutoff_date_raw = "") {
    cutoff_date <- suppressWarnings(as.Date(as.character(cutoff_date_raw)))

    resolve_snapshot_or_bundle <- function(rel_path) {
      candidates <- c(
        file.path(snapshot_root, rel_path),
        if (nzchar(bundle_root)) file.path(bundle_root, rel_path) else ""
      )
      candidates <- candidates[nzchar(candidates)]
      existing <- candidates[file.exists(candidates)]
      if (length(existing) == 0L) return("")
      normalizePath(existing[[1L]], mustWork = FALSE)
    }

    read_csv_safe <- function(path) {
      tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    }

    normalize_retros_wide <- function(df) {
      date_info <- parse_first_date_col(df)
      if (is.null(date_info)) return(NULL)
      usgs <- coerce_numeric_col(df, c("USGS", "usgs", "usgs_cms", "usgs_discharge_cms"))
      glofas <- coerce_numeric_col(df, c("GloFAS", "glofas", "glofas_cms", "selected_glofas_retrospective_value"))
      nws <- coerce_numeric_col(df, c("NWS3.0", "NWS", "nws", "nws_cms", "selected_nws_synthetic_value"))
      data.frame(
        Date = as.Date(date_info$values),
        USGS = usgs,
        GloFAS = glofas,
        NWS3.0 = nws,
        stringsAsFactors = FALSE
      )
    }

    canonical_usgs_path <- resolve_snapshot_or_bundle(file.path("inputs", "usgs_daily.csv"))

    infer_retros_scale <- function(out, usgs_path) {
      if (!nzchar(usgs_path) || !file.exists(usgs_path) || !is.data.frame(out) || nrow(out) < 10L) {
        return("unknown")
      }
      usgs <- read_csv_safe(usgs_path)
      if (!is.data.frame(usgs)) return("unknown")
      usgs_date <- parse_first_date_col(usgs)
      if (is.null(usgs_date)) return("unknown")
      usgs_ref <- data.frame(
        Date = as.Date(usgs_date$values),
        usgs_raw = coerce_numeric_col(usgs, c("discharge_cms", "USGS", "usgs")),
        stringsAsFactors = FALSE
      )
      cmp <- merge(out[, c("Date", "USGS"), drop = FALSE], usgs_ref, by = "Date", all = FALSE)
      cmp <- cmp[is.finite(cmp$USGS) & is.finite(cmp$usgs_raw), , drop = FALSE]
      if (nrow(cmp) < 10L) return("unknown")
      mae_raw <- mean(abs(cmp$USGS - cmp$usgs_raw), na.rm = TRUE)
      mae_log1p <- mean(abs(expm1(cmp$USGS) - cmp$usgs_raw), na.rm = TRUE)
      if (is.finite(mae_raw) && is.finite(mae_log1p)) {
        if (mae_raw <= mae_log1p) return("raw_cms")
        return("log1p_cms")
      }
      "unknown"
    }

    finalize_out <- function(out, usgs_path) {
      if (!is.data.frame(out)) return(NULL)
      out <- out[!is.na(out$Date), , drop = FALSE]
      if (is.finite(cutoff_date)) {
        out <- out[out$Date <= cutoff_date, , drop = FALSE]
      }
      out <- drop_incomplete_retros(out)
      if (!is.data.frame(out) || nrow(out) < 10L) return(NULL)

      scale_mode <- infer_retros_scale(out, usgs_path)
      if (identical(scale_mode, "raw_cms")) {
        for (nm in c("USGS", "GloFAS", "NWS3.0")) {
          vals <- suppressWarnings(as.numeric(out[[nm]]))
          vals[!is.finite(vals)] <- NA_real_
          vals <- pmax(vals, 1.0e-8)
          out[[nm]] <- log1p(vals)
        }
      }

      out <- floor_nonpositive_retros(out)
      assert_retros_daily_continuity(out, context_label = "forecats snapshot retrospective")
      out
    }

    parse_selection_policy <- function(bundle_root, cutoff_date) {
      glofas_priority <- c(
        "glofas_hist_v40_lisflood_cons",
        "glofas_hist_v31_lisflood_cons",
        "glofas_hist_v21_htessel_cons",
        "glofas_legacy_reanalysis_v30",
        "glofas_synth_retro_ens_mean"
      )
      nws_priority <- c(
        "nws_synth_retro_ens_mean",
        "nws_retro_v21",
        "nws_retro_v30",
        "nws_retro_v20",
        "nws_retro_v12"
      )
      meta_path <- if (nzchar(bundle_root)) file.path(bundle_root, "meta.yaml") else ""
      if (!nzchar(meta_path) || !file.exists(meta_path)) {
        return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
      }
      meta <- tryCatch(yaml::read_yaml(meta_path), error = function(e) NULL)
      if (!is.list(meta)) {
        return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
      }
      sel <- meta$config$inputs$retros$selection_policy
      if (!is.list(sel)) {
        return(list(glofas_priority = glofas_priority, nws_priority = nws_priority))
      }

      `%or_default%` <- function(x, y) {
        if (is.null(x)) return(y)
        x
      }

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
      if (length(keep_glofas) > 0L) {
        glofas_priority <- unique(c(keep_glofas, glofas_priority))
      }
      if (length(keep_nws) > 0L) {
        nws_priority <- unique(c(keep_nws, nws_priority))
      }

      win_glofas <- pick_window_source(sel$glofas_by_cutoff_windows, cutoff_date)
      win_nws <- pick_window_source(sel$nws_by_cutoff_windows, cutoff_date)
      if (nzchar(win_glofas)) {
        glofas_priority <- unique(c(win_glofas, glofas_priority))
      }
      if (nzchar(win_nws)) {
        nws_priority <- unique(c(win_nws, nws_priority))
        if (identical(win_nws, "nws_retro_v20")) {
          nws_priority <- unique(c(win_nws, "nws_retro_v21", "nws_retro_v30", nws_priority))
        } else if (identical(win_nws, "nws_retro_v21")) {
          nws_priority <- unique(c(win_nws, "nws_retro_v30", nws_priority))
        }
      }

      list(glofas_priority = glofas_priority, nws_priority = nws_priority)
    }

    choose_preferred_by_date <- function(retro_tbl, source_regex, priorities) {
      rows <- retro_tbl[grepl(source_regex, retro_tbl$source_id), c("Date", "source_id", "discharge_cms"), drop = FALSE]
      if (nrow(rows) == 0L) return(data.frame(Date = as.Date(character(0)), discharge_cms = numeric(0)))
      rows$priority <- match(rows$source_id, priorities)
      rows$priority[is.na(rows$priority)] <- length(priorities) + 1L
      rows <- rows[order(rows$Date, rows$priority, rows$source_id), , drop = FALSE]
      rows <- rows[!duplicated(rows$Date), c("Date", "discharge_cms"), drop = FALSE]
      rows
    }

    # 1) Prefer long retros_daily reconstruction with explicit source policy.
    # This guarantees cutoff-aware source selection (including synthetic NWS priority)
    # and avoids blindly trusting pre-aggregated wide files with unknown lineage.
    long_retros_path <- resolve_snapshot_or_bundle(file.path("inputs", "retros_daily.csv"))
    usgs_path <- resolve_snapshot_or_bundle(file.path("inputs", "usgs_daily.csv"))
    if (nzchar(long_retros_path) && nzchar(usgs_path) && file.exists(long_retros_path) && file.exists(usgs_path)) {
      long_retros <- read_csv_safe(long_retros_path)
      usgs <- read_csv_safe(usgs_path)
      if (is.data.frame(long_retros) && is.data.frame(usgs) &&
          ("source_id" %in% names(long_retros)) && ("discharge_cms" %in% names(long_retros))) {
        retro_date <- parse_first_date_col(long_retros)
        usgs_date <- parse_first_date_col(usgs)
        if (!is.null(retro_date) && !is.null(usgs_date)) {
          retro_tbl <- data.frame(
            Date = as.Date(retro_date$values),
            source_id = tolower(as.character(long_retros$source_id)),
            discharge_cms = suppressWarnings(as.numeric(long_retros$discharge_cms)),
            stringsAsFactors = FALSE
          )
          retro_tbl <- retro_tbl[!is.na(retro_tbl$Date) & is.finite(retro_tbl$discharge_cms), , drop = FALSE]
          if (nrow(retro_tbl) > 0L) {
            policy <- parse_selection_policy(bundle_root, cutoff_date)
            glofas_by_date <- choose_preferred_by_date(retro_tbl, "glofas", policy$glofas_priority)
            nws_by_date <- choose_preferred_by_date(retro_tbl, "^nws", policy$nws_priority)
            if (nrow(glofas_by_date) > 0L && nrow(nws_by_date) > 0L) {
              names(glofas_by_date)[2] <- "GloFAS"
              names(nws_by_date)[2] <- "NWS3.0"
              usgs_tbl <- data.frame(
                Date = as.Date(usgs_date$values),
                USGS = coerce_numeric_col(usgs, c("discharge_cms", "USGS", "usgs")),
                stringsAsFactors = FALSE
              )
              out <- merge(usgs_tbl, glofas_by_date, by = "Date", all = FALSE)
              out <- merge(out, nws_by_date, by = "Date", all = FALSE)
              out <- finalize_out(out, usgs_path)
              if (is.data.frame(out) && nrow(out) >= 10L) return(out)
            }
          }
        }
      }
    }

    # 2) Build wide retros from retrospective-preparation + USGS daily.
    prep_path <- resolve_snapshot_or_bundle(file.path("inputs", "retrospective_preparation.csv"))
    usgs_path <- resolve_snapshot_or_bundle(file.path("inputs", "usgs_daily.csv"))
    if (nzchar(prep_path) && nzchar(usgs_path) && file.exists(prep_path) && file.exists(usgs_path)) {
      prep <- read_csv_safe(prep_path)
      usgs <- read_csv_safe(usgs_path)
      if (is.data.frame(prep) && is.data.frame(usgs)) {
        prep_date <- parse_first_date_col(prep)
        usgs_date <- parse_first_date_col(usgs)
        if (!is.null(prep_date) && !is.null(usgs_date)) {
          prep_norm <- data.frame(
            Date = as.Date(prep_date$values),
            GloFAS = coerce_numeric_col(prep, c("selected_glofas_retrospective_value", "glofas", "GloFAS")),
            NWS3.0 = coerce_numeric_col(prep, c("selected_nws_synthetic_value", "nws", "NWS3.0")),
            stringsAsFactors = FALSE
          )
          usgs_norm <- data.frame(
            Date = as.Date(usgs_date$values),
            USGS = coerce_numeric_col(usgs, c("discharge_cms", "USGS", "usgs")),
            stringsAsFactors = FALSE
          )
          out <- merge(usgs_norm, prep_norm, by = "Date", all = FALSE)
          out <- finalize_out(out, usgs_path)
          if (is.data.frame(out) && nrow(out) >= 10L) return(out)
        }
      }
    }

    # 3) Fallback: prefer already-wide retros candidates if policy reconstruction
    # is unavailable.
    wide_candidates <- c("inputs/retros.csv", "inputs/retros_daily.csv")
    for (rel in wide_candidates) {
      path <- resolve_snapshot_or_bundle(rel)
      if (!nzchar(path) || !file.exists(path)) next
      dat <- read_csv_safe(path)
      if (!is.data.frame(dat)) next
      out <- normalize_retros_wide(dat)
      out <- finalize_out(out, canonical_usgs_path)
      if (is.data.frame(out) && nrow(out) >= 10L) return(out)
    }

    stop(
      paste(
        "Unable to construct snapshot retros.csv in required wide format.",
        "Expected either a wide retros CSV or",
        "inputs/retrospective_preparation.csv + inputs/usgs_daily.csv."
      ),
      call. = FALSE
    )
  }

  sanitize_member_forecast_csv <- function(src, dst, label, min_numeric_cols = 2L) {
    dat <- tryCatch(
      utils::read.csv(src, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.data.frame(dat)) {
      stop(sprintf("failed to parse %s source CSV: %s", label, src), call. = FALSE)
    }
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    if (length(numeric_cols) < min_numeric_cols) {
      stop(
        sprintf(
          "%s source CSV has too few numeric member columns (%d < %d): %s",
          label, length(numeric_cols), min_numeric_cols, src
        ),
        call. = FALSE
      )
    }
    mat <- as.matrix(dat[, numeric_cols, drop = FALSE])
    finite_col_mask <- colSums(is.finite(mat), na.rm = TRUE) > 0L
    keep_numeric <- numeric_cols[finite_col_mask]
    if (length(keep_numeric) < min_numeric_cols) {
      stop(
        sprintf(
          "%s source CSV has insufficient finite member columns after filtering (%d < %d): %s",
          label, length(keep_numeric), min_numeric_cols, src
        ),
        call. = FALSE
      )
    }
    mat_keep <- as.matrix(dat[, keep_numeric, drop = FALSE])
    row_all_finite <- rowSums(!is.finite(mat_keep), na.rm = TRUE) == 0L
    row_keep <- which(row_all_finite)
    if (length(row_keep) == 0L) {
      row_any_finite <- which(rowSums(is.finite(mat_keep), na.rm = TRUE) > 0L)
      row_keep <- row_any_finite
    }
    if (length(row_keep) == 0L) {
      stop(sprintf("%s source CSV has no rows with finite member values: %s", label, src), call. = FALSE)
    }
    non_numeric <- setdiff(names(dat), numeric_cols)
    dat_out <- dat[row_keep, c(non_numeric, keep_numeric), drop = FALSE]
    utils::write.csv(dat_out, dst, row.names = FALSE)
    invisible(normalizePath(dst, mustWork = FALSE))
  }

  resolve_member_source <- function(bundle_root, kind, cfg, repo_root) {
    stopifnot(kind %in% c("nws", "glofas"))

    derive_cache_roots <- function(bundle_root, repo_root) {
      roots <- character(0)
      if (!is.null(repo_root) && nzchar(repo_root)) {
        roots <- c(roots, normalizePath(repo_root, mustWork = FALSE))
      }
      if (!is.null(bundle_root) && nzchar(bundle_root)) {
        bundle_norm <- normalizePath(bundle_root, mustWork = FALSE)
        parts <- strsplit(bundle_norm, .Platform$file.sep, fixed = TRUE)[[1]]
        if (length(parts) > 0L) {
          parts <- parts[nzchar(parts)]
          data_idxs <- which(parts == "data")
          if (length(data_idxs) > 0L) {
            for (data_idx in data_idxs) {
              next_part <- if (data_idx < length(parts)) parts[[data_idx + 1L]] else ""
              if (!identical(next_part, "forecats_inputs") && !identical(next_part, "forecats_cache")) {
                next
              }
              if (data_idx >= 2L) {
                prefix_parts <- parts[seq_len(data_idx - 1L)]
                candidate <- do.call(file.path, as.list(c(.Platform$file.sep, prefix_parts)))
                roots <- c(roots, normalizePath(candidate, mustWork = FALSE))
              }
            }
          }
        }
      }
      roots <- unique(roots[nzchar(roots)])
      roots
    }

    bundle_candidates <- if (identical(kind, "nws")) {
      c(
        "inputs/nws_members.csv",
        "inputs/nws_members_daily.csv",
        "inputs/nws_forecast.csv",
        "nws_members.csv",
        "nws_forecast.csv"
      )
    } else {
      c(
        "inputs/glofas_members.csv",
        "inputs/glofas_members_daily.csv",
        "inputs/glofas_members_forecast.csv",
        "inputs/glofas_forecast.csv",
        "glofas_members.csv",
        "glofas_forecast.csv"
      )
    }

    bundle_paths <- normalizePath(file.path(bundle_root, bundle_candidates), mustWork = FALSE)
    bundle_paths <- bundle_paths[file.exists(bundle_paths)]
    if (length(bundle_paths) > 0L) {
      return(bundle_paths[[1]])
    }

    site_id <- cfg$site$usgs_site
    if (is.null(site_id)) site_id <- ""
    site_id <- as.character(site_id)
    cutoff <- cfg$dates$cutoff_date
    if (is.null(cutoff)) cutoff <- ""
    cutoff <- as.character(cutoff)
    if (!nzchar(site_id) || !nzchar(cutoff)) {
      return("")
    }

    cache_roots <- derive_cache_roots(bundle_root, repo_root)
    cache_matches <- character(0)
    for (cache_root in cache_roots) {
      cache_glob <- if (identical(kind, "nws")) {
        file.path(
          cache_root, "data", "forecats_cache", sprintf("site=%s", site_id),
          "run_id=*", "forecast_cache", "nws", sprintf("cutoff_date=%s", cutoff), "nws_members.csv"
        )
      } else {
        file.path(
          cache_root, "data", "forecats_cache", sprintf("site=%s", site_id),
          "run_id=*", "forecast_cache", "glofas", sprintf("issue_date=%s", cutoff), "glofas_members.csv"
        )
      }
      cache_matches <- c(cache_matches, Sys.glob(cache_glob))
    }

    pick_latest_file(cache_matches)
  }

  if (identical(mode, "use_existing")) {
    bundle <- cfg$inputs$forecats$existing_bundle_path
    if (!is.null(bundle) && nzchar(bundle) && file.exists(bundle)) {
      if (dir.exists(bundle)) {
        bundle_root <- normalizePath(bundle, mustWork = FALSE)
      } else {
        bundle_root <- normalizePath(dirname(bundle), mustWork = FALSE)
        manifest <- unified_manifest_add_artifact(
          manifest,
          normalizePath(bundle, mustWork = FALSE),
          storage_scale = "bundle",
          role = "input_snapshot"
        )
      }
    }
  }

  if (identical(mode, "build")) {
    cfg_path <- cfg$inputs$forecats$pipeline_config_path
    log_path <- file.path(fore_root, "forecats_pipeline.log")
    status <- system2(
      "Rscript",
      c("--vanilla", file.path("scripts", "forecats_pipeline.R"), "--config", cfg_path),
      stdout = log_path,
      stderr = log_path
    )
    if (!is.null(status) && status != 0L) {
      stop(sprintf("forecats stage failed; see %s", log_path), call. = FALSE)
    }

    out <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character(0)
    bundle_lines <- grep("^Bundle ready:\\s*", out, value = TRUE)
    if (length(bundle_lines) > 0L) {
      bundle_root <- sub("^Bundle ready:\\s*", "", bundle_lines[[length(bundle_lines)]])
      bundle_root <- normalizePath(bundle_root, mustWork = FALSE)
    }
    if (is.null(bundle_root) || !nzchar(bundle_root) || !dir.exists(bundle_root)) {
      fallback <- cfg$inputs$forecats$existing_bundle_path
      if (!is.null(fallback) && nzchar(fallback) && dir.exists(fallback)) {
        bundle_root <- normalizePath(fallback, mustWork = FALSE)
      }
    }
  }

  if (!identical(mode, "use_existing") && !identical(mode, "build")) {
    stop(sprintf("Unsupported forecats mode: %s", mode), call. = FALSE)
  }

  if (isTRUE(snapshot_enabled)) {
    if (is.null(bundle_root) || !nzchar(bundle_root) || !dir.exists(bundle_root)) {
      stop(
        sprintf(
          "forecats snapshot enabled but bundle root could not be determined for mode=%s. Check inputs.forecats settings.",
          mode
        ),
        call. = FALSE
      )
    }

    snapshot_root <- file.path(run_root, snapshot_dest_rel)
    if (dir.exists(snapshot_root)) {
      unlink(snapshot_root, recursive = TRUE, force = TRUE)
    }
    dir.create(snapshot_root, recursive = TRUE, showWarnings = FALSE)

    rel_files <- snapshot_copy_list
    if (length(rel_files) == 0L) {
      rel_files <- c(
        "meta.yaml",
        "inputs/retros_daily.csv",
        "inputs/nws_weighted_daily.csv",
        "inputs/glofas_weighted_daily.csv"
      )
    }

    copy_one <- function(rel) {
      dst <- file.path(snapshot_root, rel)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      src <- resolve_bundle_artifact(bundle_root, rel)
      if (!nzchar(src)) {
        if (identical(rel, "meta.yaml")) {
          writeLines(yaml::as.yaml(synthesize_bundle_meta(cfg, bundle_root)), con = dst)
        } else {
          stop(
            sprintf(
              "forecats snapshot missing required artifact for %s under bundle root %s",
              rel, bundle_root
            ),
            call. = FALSE
          )
        }
      } else {
        ok <- file.copy(src, dst, overwrite = TRUE)
        if (!isTRUE(ok) || !file.exists(dst)) {
          stop(sprintf("failed to copy forecats snapshot artifact: %s -> %s", src, dst), call. = FALSE)
        }
      }
      storage_scale <- if (grepl("retros", rel, ignore.case = TRUE)) {
        "table_csv"
      } else if (grepl("\\.csv$", dst, ignore.case = TRUE)) {
        "raw_cms"
      } else {
        "input_snapshot"
      }
      manifest <<- unified_manifest_add_artifact(
        manifest,
        normalizePath(dst, mustWork = FALSE),
        storage_scale = storage_scale,
        role = "input_snapshot"
      )
      normalizePath(dst, mustWork = FALSE)
    }

    copied <- vapply(rel_files, copy_one, character(1))
    names(copied) <- rel_files

    nws_members_src <- resolve_member_source(bundle_root, kind = "nws", cfg = cfg, repo_root = repo_root)
    if (!nzchar(nws_members_src) || !file.exists(nws_members_src)) {
      stop(
        paste0(
          "forecats snapshot could not locate member-level NWS forecast CSV. ",
          "Expected bundle member CSV or data/forecats_cache/.../forecast_cache/nws/cutoff_date=<cutoff>/nws_members.csv"
        ),
        call. = FALSE
      )
    }
    glofas_members_src <- resolve_member_source(bundle_root, kind = "glofas", cfg = cfg, repo_root = repo_root)
    if (!nzchar(glofas_members_src) || !file.exists(glofas_members_src)) {
      stop(
        paste0(
          "forecats snapshot could not locate member-level GloFAS forecast CSV. ",
          "Expected bundle member CSV or data/forecats_cache/.../forecast_cache/glofas/issue_date=<cutoff>/glofas_members.csv"
        ),
        call. = FALSE
      )
    }

    copy_external <- function(src, rel, storage_scale = "raw_cms", role = "input_snapshot") {
      dst <- file.path(snapshot_root, rel)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(src, dst, overwrite = TRUE)
      if (!isTRUE(ok) || !file.exists(dst)) {
        stop(sprintf("failed to copy forecats snapshot artifact: %s -> %s", src, dst), call. = FALSE)
      }
      manifest <<- unified_manifest_add_artifact(
        manifest,
        normalizePath(dst, mustWork = FALSE),
        storage_scale = storage_scale,
        role = role
      )
      normalizePath(dst, mustWork = FALSE)
    }

    copied[["inputs/nws_members.csv"]] <- copy_external(nws_members_src, "inputs/nws_members.csv", storage_scale = "raw_cms")
    copied[["inputs/glofas_members.csv"]] <- copy_external(glofas_members_src, "inputs/glofas_members.csv", storage_scale = "raw_cms")

    source_map_path <- file.path(snapshot_root, "snapshot_source_map.txt")
    writeLines(
      c(
        sprintf("mode=%s", mode),
        sprintf("bundle_root=%s", bundle_root),
        "publication_protocol=latest_forecast_only",
        "legacy_weighted_daily_filenames_are_aliases=true",
        sprintf("nws_members_source=%s", nws_members_src),
        sprintf("glofas_members_source=%s", glofas_members_src)
      ),
      con = source_map_path
    )
    manifest <- unified_manifest_add_artifact(
      manifest,
      normalizePath(source_map_path, mustWork = FALSE),
      storage_scale = "text",
      role = "input_snapshot"
    )

    retros_generated <- build_snapshot_retros(
      snapshot_root = snapshot_root,
      bundle_root = bundle_root,
      cutoff_date_raw = cfg$dates$cutoff_date
    )
    retros_generated_path <- file.path(snapshot_root, "retros_generated.csv")
    utils::write.csv(retros_generated, retros_generated_path, row.names = FALSE)
    manifest <- unified_manifest_add_artifact(
      manifest,
      normalizePath(retros_generated_path, mustWork = FALSE),
      storage_scale = "log1p_cms",
      role = "input_snapshot"
    )

    alias_sources <- list(
      retros = retros_generated_path,
      nws_forecast = choose_snapshot_alias_source(
        snapshot_root = snapshot_root,
        candidates = c("inputs/nws_members.csv", "inputs/nws_weighted_daily.csv", "inputs/nws_forecast.csv"),
        label = "nws_forecast",
        min_rows = 5L,
        min_numeric_cols = 2L
      ),
      glofas_forecast = choose_snapshot_alias_source(
        snapshot_root = snapshot_root,
        candidates = c("inputs/glofas_members.csv", "inputs/glofas_weighted_daily.csv", "inputs/glofas_forecast.csv"),
        label = "glofas_forecast",
        min_rows = 20L,
        min_numeric_cols = 20L
      )
    )
    unified_validate_glofas_members_csv(alias_sources$glofas_forecast, stage_name = "forecats/snapshot_alias")

    for (nm in names(alias_sources)) {
      src <- alias_sources[[nm]]
      if (!file.exists(src)) {
        stop(sprintf("forecats snapshot alias source missing for %s: %s", nm, src), call. = FALSE)
      }
      dst <- file.path(snapshot_root, sprintf("%s.csv", nm))
      if (nm %in% c("nws_forecast", "glofas_forecast")) {
        min_cols <- if (identical(nm, "glofas_forecast")) 20L else 2L
        sanitize_member_forecast_csv(src, dst, label = nm, min_numeric_cols = min_cols)
        ok <- file.exists(dst)
      } else {
        if (identical(normalizePath(src, mustWork = FALSE), normalizePath(dst, mustWork = FALSE))) {
          ok <- TRUE
        } else {
          ok <- file.copy(src, dst, overwrite = TRUE)
        }
      }
      if (!isTRUE(ok) || !file.exists(dst)) {
        stop(sprintf("failed to create forecats snapshot alias: %s -> %s", src, dst), call. = FALSE)
      }
      manifest <- unified_manifest_add_artifact(
        manifest,
        normalizePath(dst, mustWork = FALSE),
        storage_scale = if (identical(nm, "retros")) "log1p_cms" else "raw_cms",
        role = "input_snapshot"
      )
    }
  }

  list(manifest = manifest)
}
