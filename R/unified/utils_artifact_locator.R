# unified/utils_artifact_locator.R

unified_manifest_artifact_paths <- function(manifest) {
  artifacts <- manifest$artifacts
  if (is.null(artifacts) || length(artifacts) == 0L) {
    return(character(0))
  }
  paths <- vapply(artifacts, function(x) {
    val <- x$path
    if (is.null(val)) "" else as.character(val)
  }, character(1))
  paths[nzchar(paths)]
}

unified_find_artifact_paths <- function(manifest, pattern, must_exist = TRUE) {
  if (!is.character(pattern) || length(pattern) != 1L || !nzchar(pattern)) {
    stop("pattern must be a non-empty character scalar", call. = FALSE)
  }
  paths <- unified_manifest_artifact_paths(manifest)
  if (length(paths) == 0L) {
    if (isTRUE(must_exist)) {
      stop("manifest has no artifacts to search", call. = FALSE)
    }
    return(character(0))
  }
  hit <- grepl(pattern, paths, perl = TRUE)
  out <- paths[hit]
  if (isTRUE(must_exist) && length(out) == 0L) {
    stop(sprintf("no artifacts matched pattern: %s", pattern), call. = FALSE)
  }
  out
}

unified_first_artifact_path <- function(manifest, pattern, must_exist = TRUE) {
  out <- unified_find_artifact_paths(manifest, pattern = pattern, must_exist = must_exist)
  if (length(out) == 0L) {
    return("")
  }
  out[[1]]
}

unified_artifact_path_to_absolute <- function(path, run_root, repo_root = getwd(), must_exist = TRUE) {
  if (!is.character(path) || length(path) != 1L) {
    stop("path must be a character scalar", call. = FALSE)
  }
  if (!nzchar(path)) {
    if (isTRUE(must_exist)) stop("path is empty", call. = FALSE)
    return("")
  }

  run_root_abs <- normalizePath(run_root, mustWork = FALSE)
  repo_root_abs <- normalizePath(repo_root, mustWork = FALSE)

  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    out <- path.expand(path)
    if (isTRUE(must_exist) && !file.exists(out)) {
      stop(sprintf("artifact path does not exist: %s", out), call. = FALSE)
    }
    return(out)
  }

  candidates <- unique(c(
    file.path(repo_root_abs, path),
    file.path(run_root_abs, path),
    file.path(repo_root_abs, run_root, path),
    path
  ))

  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) {
    return(path.expand(existing[[1]]))
  }

  out <- path.expand(file.path(repo_root_abs, path))
  if (isTRUE(must_exist)) {
    stop(sprintf("artifact path does not exist: %s", out), call. = FALSE)
  }
  out
}

unified_artifact_paths_to_absolute <- function(paths, run_root, repo_root = getwd(), must_exist = TRUE) {
  if (length(paths) == 0L) {
    return(character(0))
  }
  vapply(
    paths,
    function(path) unified_artifact_path_to_absolute(path, run_root = run_root, repo_root = repo_root, must_exist = must_exist),
    character(1)
  )
}
