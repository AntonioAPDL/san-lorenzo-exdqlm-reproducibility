# Utilities for Environmetrics figure generation

save_plot <- function(filename, plot = NULL, ...) {
  if (!isTRUE(WRITE_FIGURES)) {
    return(invisible(FALSE))
  }

  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
  }

  target <- filename
  if (!grepl("^/|^[A-Za-z]:", filename)) {
    dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
    target <- file.path(FIG_DIR, filename)
  } else {
    dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
  }

  args <- list(filename = target, plot = plot)
  do.call(ggplot2::ggsave, c(args, list(...)))
  invisible(TRUE)
}
