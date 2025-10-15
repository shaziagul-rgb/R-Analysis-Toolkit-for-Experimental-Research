# =============================================================================
# 00_setup.R
# Shared setup, paths, packages, and plotting helpers
# =============================================================================

# Install packages once if needed:
# install.packages(c("here", "readr", "dplyr", "ggplot2", "agricolae",
#                    "ggpubr", "scales"))

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(ggplot2)
})

# All paths are relative to the project root.
# The project can therefore be moved to another computer without changing
# hard-coded user-specific paths.
DATA_DIR <- here::here("data", "raw")
FIGURE_DIR <- here::here("figures", "reconstructed")
RESULT_DIR <- here::here("results")

dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULT_DIR, recursive = TRUE, showWarnings = FALSE)

theme_phd <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "top"
    )
}

save_phd_plot <- function(plot, filename, width = 7, height = 5) {
  ggsave(
    filename = here::here("figures", "reconstructed", filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

require_columns <- function(data, columns, data_name = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s is missing required column(s): %s",
        data_name, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
