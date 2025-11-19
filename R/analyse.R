# =============================================================================
# analyse.R - the main entry point: analyse("my_data.csv")
# =============================================================================

safe_name <- function(x) gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x))

check_cols <- function(df, cols) {
  cols <- unique(cols[!is.null(cols)])
  missing <- setdiff(cols, names(df))
  if (length(missing) == 0L) return(invisible(TRUE))
  hints <- vapply(missing, function(m) {
    g <- agrep(m, names(df), ignore.case = TRUE, value = TRUE, max.distance = 0.3)
    if (length(g) > 0L) paste0("'", m, "' - did you mean '", g[1L], "'?") else
      paste0("'", m, "'")
  }, character(1))
  stop("Column not found: ", paste(hints, collapse = "; "),
       "\nAvailable columns: ", paste(names(df), collapse = ", "), call. = FALSE)
}

# Apply columns chosen by the user on top of the automatic detection.
apply_overrides <- function(plan, df, group, outcome, columns, id) {
  check_cols(df, c(group, if (!identical(outcome, "all")) outcome, id, columns))

  if (!is.null(columns)) {
    plan$mode <- "groups"
    plan$layout <- "wide"
    plan$value_cols <- columns
    plan$group <- NULL
  }
  if (!is.null(group)) {
    plan$mode <- "groups"
    plan$layout <- "long"
    plan$group <- group
    plan$outcomes <- outcome_candidates(profile_columns(df), exclude = group)
  }
  if (!is.null(id)) plan$id <- id
  if (!is.null(outcome)) {
    if (!(identical(plan$mode, "groups") && identical(plan$layout, "long"))) {
      stop("An outcome column was given, but no grouping column could be ",
           "detected. Please also supply group = \"<column name>\".",
           call. = FALSE)
    }
    if (identical(outcome, "all")) {
      plan$use_all <- TRUE
    } else {
      plan$outcomes <- outcome
      plan$use_all <- TRUE
    }
  }
  plan
}

finalize_plan <- function(plan) {
  if (identical(plan$mode, "groups") && identical(plan$layout, "long")) {
    if (length(plan$outcomes) == 0L) {
      stop("No numeric outcome column is available.", call. = FALSE)
    }
    plan$selected <- if (isTRUE(plan$use_all)) plan$outcomes else plan$outcomes[1L]
  }
  plan
}

describe_plan <- function(plan) {
  lines <- switch(plan$mode,
    groups = if (plan$layout == "long") {
      c(sprintf("Detected: compare groups defined by '%s'.", plan$group),
        sprintf("Outcome column(s) analysed: %s.",
                paste0("'", plan$selected, "'", collapse = ", ")),
        if (length(plan$outcomes) > length(plan$selected)) {
          sprintf(paste0("Other numeric columns not analysed: %s ",
                         "(use outcome = \"all\" or name one)."),
                  paste(setdiff(plan$outcomes, plan$selected), collapse = ", "))
        })
    } else {
      sprintf("Detected: each of these columns is one condition: %s.",
              paste(plan$value_cols, collapse = ", "))
    },
    summary = sprintf("Detected: a summary table (labels in '%s', values in '%s').",
                      plan$label, plan$value),
    frequency = sprintf("Detected: categorical data only; counting categories in '%s'.",
                        plan$category),
    distribution = sprintf("Detected: a single numeric column '%s'.", plan$column)
  )
  c(lines, plan$notes)
}

# Let the user accept or change the detected columns (interactive sessions only).
confirm_plan <- function(df, plan) {
  if (!interactive()) return(plan)
  cat("\n", paste(describe_plan(finalize_plan(plan)), collapse = "\n"), "\n\n", sep = "")
  ans <- utils::menu(c("Yes, use these", "No, let me choose the columns"),
                     title = "Use the detected settings?")
  if (ans != 2L) return(plan)

  grp <- utils::select.list(
    names(df), graphics = TRUE,
    title = "Which column defines the groups/conditions? (Cancel if each group is its own column)")
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (nzchar(grp)) {
    out <- utils::select.list(setdiff(num_cols, grp), graphics = TRUE,
                              title = "Which column holds the numeric outcome?")
    if (!nzchar(out)) stop("No outcome column selected.", call. = FALSE)
    plan$mode <- "groups"; plan$layout <- "long"; plan$group <- grp
    plan$outcomes <- out; plan$use_all <- TRUE
  } else {
    vals <- utils::select.list(num_cols, multiple = TRUE, graphics = TRUE,
                               title = "Select the columns to compare")
    if (length(vals) < 2L) stop("Select at least two columns.", call. = FALSE)
    plan$mode <- "groups"; plan$layout <- "wide"; plan$value_cols <- vals
  }
  plan
}

round_df <- function(d, digits = 3) {
  d[] <- lapply(d, function(x) if (is.numeric(x)) round(x, digits) else x)
  d
}

build_report <- function(res, header, alpha) {
  out <- c(header, strrep("=", 64), "", "DESCRIPTIVE STATISTICS",
           utils::capture.output(print(round_df(res$descriptives), row.names = FALSE)),
           "")
  if (!is.null(res[["assumptions"]])) {
    a <- res[["assumptions"]]
    out <- c(out, "ASSUMPTION CHECKS (Shapiro-Wilk normality)",
             utils::capture.output(print(round_df(a$table, 4), row.names = FALSE)))
    if (!is.na(a$var_p)) {
      out <- c(out, sprintf("Fligner-Killeen test of equal variances: %s", p_text(a$var_p)))
    }
    out <- c(out, "")
  }
  if (!is.null(res[["summary_text"]])) {
    out <- c(out, "TEST", res[["summary_text"]],
             sprintf("N = %d, groups = %d, paired = %s",
                     nrow(res$data), nlevels(res$data$Group), res$paired), "")
  }
  if (!is.null(res[["posthoc"]])) {
    out <- c(out, paste0("POST HOC: ", res$posthoc_method),
             utils::capture.output(print(round_df(res$posthoc, 4), row.names = FALSE)),
             "")
  }
  if (!is.null(res[["letters"]])) {
    out <- c(out, "SIGNIFICANCE LETTERS (groups sharing a letter do not differ)",
             paste0("  ", names(res$letters), ": ", res$letters), "")
  }
  if (length(res$notes) > 0L) out <- c(out, "NOTES", paste0("- ", res$notes), "")
  out
}

analyse_one <- function(data, paired, stem, group_label, outcome_label, opts,
                        outdir) {
  res <- compare_groups(data, paired = paired, test = opts$test,
                        alpha = opts$alpha, p_adjust = opts$p_adjust)
  lv <- levels(res$data$Group)

  caption <- res$summary_text
  if (!is.null(res[["letters"]])) {
    caption <- paste0(caption, "\nGroups sharing a letter do not differ ",
                      "significantly (alpha = ", opts$alpha, ", ",
                      res$posthoc_method, ").")
  }

  p <- plot_groups(
    res$data, res$descriptives, group_letters = res[["letters"]],
    group_label = group_label, outcome_label = outcome_label,
    title = opts$title %||% paste(outcome_label, "by", group_label),
    caption = caption, show_points = opts$show_points, log_y = opts$log_y)

  size <- suggest_size(lv)
  save_figure(p, stem, outdir, size$width, size$height, opts$formats)

  utils::write.csv(res$descriptives,
                   file.path(outdir, paste0(stem, "_descriptives.csv")),
                   row.names = FALSE)
  if (!is.null(res[["test_table"]])) {
    utils::write.csv(res[["test_table"]],
                     file.path(outdir, paste0(stem, "_test.csv")), row.names = FALSE)
  }
  if (!is.null(res[["posthoc"]])) {
    utils::write.csv(res[["posthoc"]],
                     file.path(outdir, paste0(stem, "_posthoc.csv")), row.names = FALSE)
  }
  report <- build_report(res, paste("Analysis:", outcome_label, "by", group_label),
                         opts$alpha)
  writeLines(report, file.path(outdir, paste0(stem, "_report.txt")))
  cat(paste(report, collapse = "\n"), "\n", sep = "")

  v <- data$Value[!is.na(data$Value)]
  if (!isTRUE(opts$log_y) && min(v) > 0 && stats::median(v) > 0 &&
      max(v) / stats::median(v) > 50) {
    message("Tip: values span a very wide range, so boxes may look flat. ",
            "Try log_y = TRUE for a clearer figure.")
  }
  res$plot <- p
  res
}

run_groups_mode <- function(df, plan, opts, outdir) {
  if (identical(plan$layout, "wide")) {
    data <- prepare_wide(df, plan$value_cols)
    paired <- opts$paired
    if (is.null(paired)) {
      counts <- vapply(plan$value_cols,
                       function(cn) as.numeric(sum(!is.na(df[[cn]]))), numeric(1))
      paired <- all(counts == nrow(df))
      message(if (paired) {
        "Every row has a value in every column, so rows are treated as the same subject (paired). Use paired = FALSE for independent samples."
      } else {
        "Columns have different numbers of values, so they are treated as independent samples. Use paired = TRUE if rows are matched subjects."
      })
    }
    return(list(analyse_one(
      data, paired, "groups_comparison", opts$xlab %||% "Condition",
      opts$ylab %||% "Value", opts, outdir)))
  }

  lapply(plan$selected, function(o) {
    if (!is.numeric(df[[o]])) {
      stop("Outcome column '", o, "' is not numeric.", call. = FALSE)
    }
    data <- prepare_long(df, plan$group, o, id = plan$id,
                         group_order = opts$group_order)
    paired <- opts$paired
    if (is.null(paired)) {
      paired <- FALSE
      if (!is.null(plan$id) && "Subject" %in% names(data) && nrow(data) > 0L) {
        tab <- table(data$Subject, droplevels(data$Group))
        if (nlevels(droplevels(data$Group)) >= 2L && all(tab == 1L)) {
          paired <- TRUE
          message("Each '", plan$id, "' appears once in every group, so a ",
                  "paired analysis is used.")
        }
      }
    }
    message("\n--- Outcome: ", o, " ---")
    analyse_one(data, paired, paste0("groups_", safe_name(o)),
                opts$xlab %||% plan$group, opts$ylab %||% o, opts, outdir)
  })
}

run_summary_mode <- function(df, plan, opts, outdir) {
  vals <- df[[plan$value]]
  labs <- as.character(df[[plan$label]])
  keep <- !is.na(vals)
  vals <- vals[keep]
  labs <- labs[keep]
  is_pct <- grepl("percent|pct|%", tolower(plan$value)) || abs(sum(vals) - 100) < 1
  p <- plot_bars(labs, vals, ylab = opts$ylab %||% plan$value,
                 title = opts$title %||% paste(plan$value, "by", plan$label),
                 percent = is_pct, sort_bars = opts$sort_bars)
  stem <- paste0("bars_", safe_name(plan$value))
  save_figure(p, stem, outdir, formats = opts$formats)
  utils::write.csv(data.frame(Label = labs, Value = vals),
                   file.path(outdir, paste0(stem, "_data.csv")), row.names = FALSE)
  list(list(plot = p))
}

run_frequency_mode <- function(df, plan, opts, outdir) {
  x <- df[[plan$category]]
  x <- x[!is.na(x)]
  lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
  counts <- as.numeric(table(factor(as.character(x), levels = lv)))
  pct <- 100 * counts / sum(counts)
  p <- plot_bars(lv, pct, ylab = opts$ylab %||% "Share of responses (%)",
                 title = opts$title %||% plan$category,
                 subtitle = paste0("n = ", sum(counts)), percent = TRUE,
                 sort_bars = opts$sort_bars)
  stem <- paste0("frequency_", safe_name(plan$category))
  save_figure(p, stem, outdir, formats = opts$formats)
  tab <- data.frame(Category = lv, Count = counts, Percent = round(pct, 2))
  utils::write.csv(tab, file.path(outdir, paste0(stem, "_table.csv")), row.names = FALSE)
  print(tab, row.names = FALSE)
  list(list(plot = p, table = tab))
}

run_distribution_mode <- function(df, plan, opts, outdir) {
  v <- df[[plan$column]]
  p <- plot_histogram(v, xlab = opts$xlab %||% plan$column,
                      title = opts$title %||% paste("Distribution of", plan$column))
  stem <- paste0("distribution_", safe_name(plan$column))
  save_figure(p, stem, outdir, formats = opts$formats)
  desc <- data.frame(Variable = plan$column, N = sum(!is.na(v)), Mean = mean(v, na.rm = TRUE),
                     SD = stats::sd(v, na.rm = TRUE), Median = stats::median(v, na.rm = TRUE),
                     Min = min(v, na.rm = TRUE), Max = max(v, na.rm = TRUE))
  utils::write.csv(desc, file.path(outdir, paste0(stem, "_descriptives.csv")), row.names = FALSE)
  print(round_df(desc), row.names = FALSE)
  list(list(plot = p, descriptives = desc))
}

#' Analyse a data file: detect its structure, run suitable statistics, plot.
#'
#' @param path       Path to a CSV/TSV/TXT/XLSX/XLS file. If NULL in an
#'                   interactive session a file chooser opens.
#' @param group      (optional) name of the grouping/condition column.
#' @param outcome    (optional) name(s) of the numeric outcome column(s), or
#'                   "all" to analyse every numeric column.
#' @param id         (optional) subject/participant column (enables paired tests).
#' @param columns    (optional) character vector of columns that are each one
#'                   condition ("wide" layout), e.g. c("Manual", "Auto").
#' @param paired     TRUE/FALSE to force paired/independent analysis (default:
#'                   detected from the data).
#' @param test       "auto" (default), "parametric" or "nonparametric".
#' @param outdir     Folder for results (default: outputs/<file name>/).
#' @param log_y      Use a log scale on the y axis.
#' @param show_points "auto" (default: points if <= 150 per group), TRUE or FALSE.
#' @param title,xlab,ylab  Override plot title and axis labels.
#' @param group_order Character vector giving the order of groups on the x axis.
#' @param alpha      Significance level (default 0.05).
#' @param p_adjust   Multiple-comparison adjustment for post hoc tests.
#' @param sheet      Sheet of an Excel file (name or number).
#' @param sort_bars  Sort bars by value in bar charts.
#' @param confirm    In interactive sessions, ask before using detected columns.
#' @param formats    Figure formats to write (default png and pdf).
#' @return Invisibly, a list of results (one element per analysis).
analyse <- function(path = NULL, group = NULL, outcome = NULL, id = NULL,
                    columns = NULL, paired = NULL, test = "auto", outdir = NULL,
                    log_y = FALSE, show_points = "auto", title = NULL,
                    xlab = NULL, ylab = NULL, group_order = NULL, alpha = 0.05,
                    p_adjust = "holm", sheet = 1, sort_bars = FALSE,
                    confirm = interactive(), formats = c("png", "pdf")) {
  if (is.null(path)) {
    if (!interactive()) stop("Please provide a file path.", call. = FALSE)
    message("Choose your data file...")
    path <- file.choose()
  }

  df <- read_any_file(path, sheet = sheet)
  if (nrow(df) == 0L || ncol(df) == 0L) {
    stop("The file contains no usable data.", call. = FALSE)
  }
  message(sprintf("Loaded '%s': %d rows, %d columns.", basename(path),
                  nrow(df), ncol(df)))

  plan <- detect_structure(df)
  plan <- apply_overrides(plan, df, group, outcome, columns, id)
  if (isTRUE(confirm)) plan <- confirm_plan(df, plan)
  plan <- finalize_plan(plan)
  message(paste(describe_plan(plan), collapse = "\n"))

  if (is.null(outdir)) {
    outdir <- file.path("outputs", tools::file_path_sans_ext(basename(path)))
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  opts <- list(paired = paired, test = test, log_y = log_y,
               show_points = show_points, title = title, xlab = xlab,
               ylab = ylab, group_order = group_order, alpha = alpha,
               p_adjust = p_adjust, sort_bars = sort_bars, formats = formats)

  results <- switch(plan$mode,
    groups = run_groups_mode(df, plan, opts, outdir),
    summary = run_summary_mode(df, plan, opts, outdir),
    frequency = run_frequency_mode(df, plan, opts, outdir),
    distribution = run_distribution_mode(df, plan, opts, outdir)
  )

  message("\nResults saved to: ", normalizePath(outdir))
  invisible(results)
}
