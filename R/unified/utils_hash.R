# unified/utils_hash.R

unified_sha256 <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  if (dir.exists(path)) {
    return(NA_character_)
  }

  out <- tryCatch(
    system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  if (length(out) > 0 && !grepl("not found", out[1], fixed = TRUE)) {
    return(strsplit(out[1], "\\s+")[[1]][1])
  }

  out <- tryCatch(
    system2("openssl", c("dgst", "-sha256", shQuote(path)), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  if (length(out) > 0) {
    return(sub("^.*=\\s*", "", out[1]))
  }

  stop("Unable to compute sha256 for path: ", path)
}

unified_hash_records <- function(paths, storage_scales) {
  stopifnot(length(paths) == length(storage_scales))
  records <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    records[[i]] <- list(
      path = paths[[i]],
      sha256 = unified_sha256(paths[[i]]),
      storage_scale = storage_scales[[i]]
    )
  }
  records
}
