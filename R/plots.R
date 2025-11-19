# =============================================================================
# plots.R - publication-style figures
# =============================================================================
# Requires ggplot2 (>= 3.3.0), attached by load_project().

# Colour-blind-safe palette (Okabe-Ito); falls back to hcl colours for many groups.
group_palette <- function(k) {
  base <- c("#0072B2", "#E69F00", "#009E73", "#D55E00",
            "#CC79A7", "#56B4E9", "#F0E442", "#999999")
  if (k <= length(base)) base[seq_len(k)] else grDevices::hcl.colors(k, "Dark 3")
}

theme_publication <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0, size = rel(0.75), colour = "grey30"),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

needs_rotation <- function(levels_) length(levels_) > 6 || max(nchar(levels_)) > 10

suggest_size <- function(levels_) {
  list(width = max(5, min(14, 2.5 + 0.9 * length(levels_))),
       height = if (needs_rotation(levels_)) 5.8 else 5)
}

# Box plot of Value by Group with individual points, group means and
# (optionally) significance letters.
plot_groups <- function(data, descriptives, group_letters = NULL,
                        group_label = "Group", outcome_label = "Value",
                        title = NULL, subtitle = NULL, caption = NULL,
                        show_points = "auto", log_y = FALSE,
                        label_means = TRUE, digits = 2, base_size = 12) {
  data <- data[!is.na(data$Value), , drop = FALSE]
  data$Group <- droplevels(data$Group)
  lv <- levels(data$Group)
  k <- length(lv)

  points_on <- if (identical(show_points, "auto")) {
    max(table(data$Group)) <= 150
  } else {
    isTRUE(show_points)
  }
  use_log <- isTRUE(log_y) && max(data$Value) > 0

  rng <- range(data$Value)
  span <- if (diff(rng) > 0) diff(rng) else max(abs(rng[2L]), 1)
  label_y <- if (use_log) rng[2L] * 1.4 else rng[2L] + 0.06 * span

  means <- descriptives
  means$Group <- factor(means$Group, levels = lv)
  label <- paste0("Mean = ", formatC(means$Mean, format = "f", digits = digits))
  if (!is.null(group_letters)) {
    label <- paste0(label, "\n", unname(group_letters[as.character(means$Group)]))
  }
  means$label <- label
  means$y <- label_y

  p <- ggplot(data, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(width = 0.6, alpha = 0.75,
                 outlier.shape = if (points_on) NA else 19,
                 outlier.size = 1, outlier.alpha = 0.4)

  if (points_on) {
    p <- p + geom_jitter(width = 0.12, alpha = 0.45, size = 1,
                         show.legend = FALSE)
  }

  p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                        fill = "white", colour = "black", show.legend = FALSE)

  if (label_means) {
    p <- p + geom_text(data = means, aes(x = Group, y = y, label = label),
                       inherit.aes = FALSE, vjust = 0, size = 3.4,
                       lineheight = 0.95)
  }

  p <- p + scale_fill_manual(values = stats::setNames(group_palette(k), lv))

  if (use_log) {
    if (min(data$Value) > 0) {
      p <- p + scale_y_log10(expand = expansion(mult = c(0.05, 0.25)))
    } else {
      p <- p + scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        expand = expansion(mult = c(0.05, 0.25)))
    }
  } else {
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.18)))
  }

  p <- p +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = group_label, y = outcome_label) +
    theme_publication(base_size)

  if (needs_rotation(lv)) {
    p <- p + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  }
  p
}

# Bar chart for labelled values (e.g. percentages or counts).
plot_bars <- function(labels, values, ylab = "Value", title = NULL,
                      subtitle = NULL, percent = FALSE, digits = 1,
                      sort_bars = FALSE) {
  labels <- as.character(labels)
  order_ <- if (sort_bars) labels[order(values)] else unique(labels)
  horizontal <- length(labels) > 6 || max(nchar(labels)) > 10
  lv <- if (horizontal && !sort_bars) rev(order_) else order_

  df <- data.frame(Label = factor(labels, levels = lv), Value = values,
                   stringsAsFactors = FALSE)
  df$Text <- paste0(formatC(df$Value, format = "f", digits = digits),
                    if (percent) "%" else "")

  p <- ggplot(df, aes(x = Label, y = Value, fill = Label)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = stats::setNames(group_palette(length(lv)), lv)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) +
    theme_publication()

  if (horizontal) {
    p <- p +
      geom_text(aes(label = Text), hjust = -0.15, size = 3.6) +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
  } else {
    p <- p +
      geom_text(aes(label = Text), vjust = -0.4, size = 3.6) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
  }
  p
}

plot_histogram <- function(values, xlab = "Value", title = NULL) {
  df <- data.frame(Value = values[!is.na(values)])
  bins <- min(50, max(10, ceiling(log2(nrow(df)) + 1)))
  ggplot(df, aes(x = Value)) +
    geom_histogram(bins = bins, fill = "#0072B2", colour = "white", alpha = 0.85) +
    geom_vline(xintercept = stats::median(df$Value), linetype = "dashed") +
    labs(title = title, x = xlab, y = "Count", caption = "Dashed line = median") +
    theme_publication()
}

save_figure <- function(plot, stem, outdir, width = 7, height = 5,
                        formats = c("png", "pdf")) {
  for (f in formats) {
    ggsave(file.path(outdir, paste0(stem, ".", f)), plot = plot,
           width = width, height = height, units = "in", dpi = 300, bg = "white")
  }
  invisible(file.path(outdir, paste0(stem, ".", formats)))
}
