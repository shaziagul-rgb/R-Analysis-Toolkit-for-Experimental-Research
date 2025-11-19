# =============================================================================
# load.R - attach ggplot2 and load all project functions
# =============================================================================
load_project <- function(root = getwd()) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 first: install.packages(\"ggplot2\")", call. = FALSE)
  }
  suppressPackageStartupMessages(library(ggplot2))
  for (f in c("io.R", "detect.R", "stats.R", "plots.R", "analyse.R")) {
    source(file.path(root, "R", f), local = FALSE)
  }
  invisible(TRUE)
}
