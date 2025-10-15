# =============================================================================
# Interactive R Analysis Tool
# =============================================================================
#
# PURPOSE
# -------
# This script lets a researcher select their own data file and choose columns
# interactively. Nothing is hard-coded to a particular dataset.
#
# SUPPORTED FILES
# ---------------
# - CSV
# - TXT / TSV
# - XLSX
# - XLS
#
# WORKFLOWS
# ---------
# 1. Generic group comparison:
#    Select a grouping/category column and a numeric outcome column.
#    Produces descriptive statistics and a boxplot.
#
# 2. NASA-TLX style analysis:
#    Select the condition/group column and the overall workload column.
#    Produces descriptive statistics and a boxplot with group means.
#
# REQUIREMENTS
# ------------
# install.packages(c("readr", "readxl", "dplyr", "ggplot2", "here"))
#
# HOW TO USE
# ----------
# Run this script from RStudio. A file-selection window will open.
# The script then asks you to select the relevant columns.
#
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_packages <- c(
  "readr",
  "readxl",
  "dplyr",
  "ggplot2",
  "here"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(here)

# -----------------------------------------------------------------------------
# 2. General settings
# -----------------------------------------------------------------------------

OUTPUT_DIR <- here::here("figures", "reconstructed")
RESULTS_DIR <- here::here("results")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 3. Helper: choose and read the user's file
# -----------------------------------------------------------------------------

choose_data_file <- function() {

  message("Select your data file.")

  file_path <- file.choose()

  extension <- tolower(tools::file_ext(file_path))

  data <- switch(
    extension,

    csv = readr::read_csv(
      file_path,
      show_col_types = FALSE
    ),

    txt = readr::read_delim(
      file_path,
      delim = "\t",
      show_col_types = FALSE
    ),

    tsv = readr::read_tsv(
      file_path,
      show_col_types = FALSE
    ),

    xlsx = readxl::read_excel(file_path),

    xls = readxl::read_excel(file_path),

    stop(
      "Unsupported file type: .",
      extension,
      "\nSupported types: CSV, TXT, TSV, XLSX and XLS."
    )
  )

  if (nrow(data) == 0) {
    stop("The selected file contains no rows.")
  }

  message("\nFile loaded successfully:")
  message("  ", basename(file_path))
  message("Rows: ", nrow(data))
  message("Columns: ", ncol(data), "\n")

  data
}

# -----------------------------------------------------------------------------
# 4. Helper: interactively select a column
# -----------------------------------------------------------------------------

choose_column <- function(data, prompt, numeric_only = FALSE) {

  candidates <- names(data)

  if (numeric_only) {
    candidates <- candidates[
      vapply(
        data,
        function(x) is.numeric(x) || is.integer(x),
        logical(1)
      )
    ]

    if (length(candidates) == 0) {
      stop("No numeric columns were found in the selected dataset.")
    }
  }

  if (length(candidates) == 1) {
    message(prompt, ": ", candidates)
    return(candidates)
  }

  selection <- utils::select.list(
    candidates,
    title = prompt,
    multiple = FALSE,
    graphics = TRUE
  )

  if (!nzchar(selection)) {
    stop("No column was selected.")
  }

  selection
}

# -----------------------------------------------------------------------------
# 5. Plotting theme
# -----------------------------------------------------------------------------

theme_research <- function(base_size = 12) {

  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

# -----------------------------------------------------------------------------
# 6. Generic group comparison
# -----------------------------------------------------------------------------

run_group_comparison <- function(data, group_column, outcome_column) {

  analysis_data <- data %>%
    dplyr::select(
      Group = all_of(group_column),
      Outcome = all_of(outcome_column)
    ) %>%
    filter(
      !is.na(Group),
      !is.na(Outcome)
    ) %>%
    mutate(Group = as.factor(Group))

  if (nrow(analysis_data) == 0) {
    stop("No complete observations remain after removing missing values.")
  }

  summary_table <- analysis_data %>%
    group_by(Group) %>%
    summarise(
      N = n(),
      Mean = mean(Outcome),
      SD = sd(Outcome),
      Median = median(Outcome),
      Min = min(Outcome),
      Max = max(Outcome),
      .groups = "drop"
    )

  print(summary_table)

  p <- ggplot(
    analysis_data,
    aes(x = Group, y = Outcome, fill = Group)
  ) +
    geom_boxplot(
      width = 0.65,
      alpha = 0.75,
      outlier.alpha = 0.35
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      size = 3,
      fill = "white",
      colour = "black"
    ) +
    labs(
      title = paste("Outcome by", group_column),
      subtitle = paste("Outcome:", outcome_column),
      x = group_column,
      y = outcome_column
    ) +
    theme_research()

  print(p)

  output_name <- paste0(
    "group_comparison_",
    gsub("[^A-Za-z0-9]+", "_", outcome_column),
    ".png"
  )

  ggsave(
    filename = file.path(OUTPUT_DIR, output_name),
    plot = p,
    width = 7,
    height = 5,
    dpi = 300,
    bg = "white"
  )

  readr::write_csv(
    summary_table,
    file.path(
      RESULTS_DIR,
      paste0("summary_", gsub("[^A-Za-z0-9]+", "_", outcome_column), ".csv")
    )
  )

  invisible(list(
    data = analysis_data,
    summary = summary_table,
    plot = p
  ))
}

# -----------------------------------------------------------------------------
# 7. Main interactive workflow
# -----------------------------------------------------------------------------

cat("\n============================================\n")
cat(" Interactive R Analysis Tool\n")
cat("============================================\n\n")

data <- choose_data_file()

cat("\nAvailable columns:\n")
print(names(data))

cat("\nChoose an analysis:\n")
cat("1 = Generic group comparison\n")
cat("2 = NASA-TLX / workload analysis\n\n")

analysis_choice <- menu(
  c(
    "Generic group comparison",
    "NASA-TLX / workload analysis"
  ),
  title = "Select analysis"
)

if (analysis_choice == 0) {
  stop("No analysis selected.")
}

# Both workflows intentionally use user-selected columns.
# NASA-TLX is not tied to names such as 'Registration' or 'WorkLoad'.

group_column <- choose_column(
  data,
  "Select the GROUP / CONDITION column"
)

outcome_column <- choose_column(
  data,
  "Select the NUMERIC OUTCOME column",
  numeric_only = TRUE
)

if (analysis_choice == 2) {

  message("\nNASA-TLX analysis selected.")
  message("The selected outcome should normally be your overall NASA-TLX score.")
  message("The selected group should normally represent the experimental condition.\n")

  result <- run_group_comparison(
    data,
    group_column,
    outcome_column
  )

} else {

  message("\nGeneric group comparison selected.")

  result <- run_group_comparison(
    data,
    group_column,
    outcome_column
  )
}

cat("\n============================================\n")
cat("Analysis completed successfully.\n")
cat("Figure saved to: ", OUTPUT_DIR, "\n", sep = "")
cat("Results saved to: ", RESULTS_DIR, "\n", sep = "")
cat("============================================\n")
