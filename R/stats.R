# =============================================================================
# stats.R - descriptive statistics and automatic test selection
# =============================================================================
# Input to compare_groups() is a data frame with columns
#   Group   (factor), Value (numeric) and optionally Subject (for paired data).
#
# Decision rules (all reported in the output files):
#   2 groups, independent : Welch t-test      or Mann-Whitney U (Wilcoxon rank-sum)
#   2 groups, paired      : paired t-test     or Wilcoxon signed-rank
#   >2 groups, independent: one-way ANOVA + Tukey HSD, or Kruskal-Wallis +
#                           pairwise Wilcoxon (Holm adjusted)
#   >2 groups, paired     : Friedman test + pairwise paired Wilcoxon (Holm)
# "auto" uses the parametric test only if Shapiro-Wilk does not reject normality
# (and, for >2 independent groups, the Fligner-Killeen test does not reject
# equal variances); otherwise the non-parametric alternative is used.

p_text <- function(p) {
  if (is.na(p)) "p = NA" else if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
}

describe_groups <- function(data) {
  sp <- split(data$Value, data$Group)
  out <- lapply(names(sp), function(g) {
    v <- sp[[g]]
    n <- length(v)
    data.frame(
      Group = g, N = n, Mean = mean(v),
      SD = if (n > 1L) stats::sd(v) else NA_real_,
      SEM = if (n > 1L) stats::sd(v) / sqrt(n) else NA_real_,
      Median = stats::median(v),
      Q1 = unname(stats::quantile(v, 0.25)),
      Q3 = unname(stats::quantile(v, 0.75)),
      Min = min(v), Max = max(v),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Build the long data frame from a "long" file (group column + outcome column).
prepare_long <- function(df, group, outcome, id = NULL, group_order = NULL) {
  raw_group <- df[[group]]
  out <- data.frame(Group = as.character(raw_group),
                    Value = as.numeric(df[[outcome]]),
                    stringsAsFactors = FALSE)
  if (!is.null(id)) out$Subject <- as.character(df[[id]])
  out <- out[!is.na(raw_group) & !is.na(out$Value), , drop = FALSE]

  lv <- if (is.factor(raw_group)) {
    levels(raw_group)
  } else if (is.numeric(raw_group)) {
    as.character(sort(unique(raw_group)))
  } else {
    unique(as.character(raw_group))
  }
  lv <- lv[lv %in% out$Group]
  if (!is.null(group_order)) {
    lv <- c(intersect(group_order, lv), setdiff(lv, group_order))
  }
  out$Group <- factor(out$Group, levels = lv)
  out
}

# Build the long data frame from a "wide" file (one column per condition).
prepare_wide <- function(df, value_cols) {
  n <- nrow(df)
  data.frame(
    Group = factor(rep(value_cols, each = n), levels = value_cols),
    Value = as.numeric(unlist(df[value_cols], use.names = FALSE)),
    Subject = rep(seq_len(n), times = length(value_cols)),
    stringsAsFactors = FALSE
  )
}

# Keep only subjects that have exactly one value in every group, sorted so that
# observations line up across groups. Returns NULL if pairing is impossible.
complete_blocks <- function(data) {
  data <- data[!is.na(data$Value), , drop = FALSE]
  data$Group <- droplevels(data$Group)
  k <- nlevels(data$Group)
  tab <- table(data$Subject, data$Group)
  if (any(tab > 1L)) return(NULL)
  keep <- rownames(tab)[rowSums(tab == 1L) == k]
  out <- data[as.character(data$Subject) %in% keep, , drop = FALSE]
  out[order(out$Group, as.character(out$Subject)), , drop = FALSE]
}

check_assumptions <- function(data, paired, alpha) {
  lv <- levels(data$Group)

  shapiro_one <- function(v, label) {
    n <- length(v)
    if (n < 3L || length(unique(v)) < 2L) {
      return(data.frame(Target = label, N = n, W = NA_real_, p = NA_real_,
                        stringsAsFactors = FALSE))
    }
    sub <- if (n > 5000L) v[round(seq(1, n, length.out = 5000L))] else v
    r <- stats::shapiro.test(sub)
    data.frame(Target = label, N = n, W = unname(r$statistic),
               p = r$p.value, stringsAsFactors = FALSE)
  }

  if (paired && length(lv) == 2L) {
    x <- data$Value[data$Group == lv[1L]]
    y <- data$Value[data$Group == lv[2L]]
    tab <- shapiro_one(x - y, "Paired differences")
  } else {
    tab <- do.call(rbind, lapply(lv, function(g) {
      shapiro_one(data$Value[data$Group == g], g)
    }))
  }

  tested <- tab$p[!is.na(tab$p)]
  normal_ok <- length(tested) > 0L && all(tested > alpha)

  var_p <- NA_real_
  var_ok <- TRUE
  if (!paired && length(lv) > 2L) {
    var_p <- tryCatch(stats::fligner.test(Value ~ Group, data = data)$p.value,
                      error = function(e) NA_real_)
    if (!is.na(var_p)) var_ok <- var_p > alpha
  }
  list(table = tab, normal_ok = normal_ok, var_p = var_p, var_ok = var_ok)
}

run_two_groups <- function(data, paired, parametric) {
  lv <- levels(data$Group)
  x <- data$Value[data$Group == lv[1L]]
  y <- data$Value[data$Group == lv[2L]]

  if (paired) {
    d <- x - y
    if (parametric) {
      r <- stats::t.test(x, y, paired = TRUE)
      test <- list(name = "Paired t-test", stat_name = "t",
                   stat = unname(r$statistic),
                   df = format(round(unname(r$parameter), 2)), p = r$p.value,
                   effect_name = "Cohen's dz",
                   effect = mean(d) / stats::sd(d))
    } else {
      r <- suppressWarnings(stats::wilcox.test(x, y, paired = TRUE, exact = FALSE))
      nz <- sum(d != 0)
      v <- unname(r$statistic)
      test <- list(name = "Wilcoxon signed-rank test (paired)", stat_name = "V",
                   stat = v, df = "", p = r$p.value,
                   effect_name = "Rank-biserial correlation",
                   effect = if (nz > 0) 4 * v / (nz * (nz + 1)) - 1 else NA_real_)
    }
  } else {
    n1 <- length(x)
    n2 <- length(y)
    if (parametric) {
      r <- stats::t.test(x, y)
      sp <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) /
                   (n1 + n2 - 2))
      test <- list(name = "Welch two-sample t-test", stat_name = "t",
                   stat = unname(r$statistic),
                   df = format(round(unname(r$parameter), 2)), p = r$p.value,
                   effect_name = "Cohen's d",
                   effect = (mean(x) - mean(y)) / sp)
    } else {
      r <- suppressWarnings(stats::wilcox.test(x, y, exact = FALSE))
      w <- unname(r$statistic)
      test <- list(name = "Mann-Whitney U test (Wilcoxon rank-sum)",
                   stat_name = "W", stat = w, df = "", p = r$p.value,
                   effect_name = "Rank-biserial correlation",
                   effect = 2 * w / (n1 * n2) - 1)
    }
  }
  list(test = test, PM = NULL, post = NULL, post_method = NULL)
}

fill_pm <- function(PM, pm) {
  for (i in seq_len(nrow(pm))) {
    for (j in seq_len(ncol(pm))) {
      if (!is.na(pm[i, j])) {
        a <- rownames(pm)[i]
        b <- colnames(pm)[j]
        PM[a, b] <- pm[i, j]
        PM[b, a] <- pm[i, j]
      }
    }
  }
  PM
}

lower_index <- function(PM) {
  idx <- which(lower.tri(PM), arr.ind = TRUE)
  dimnames(idx) <- NULL
  idx
}

pm_table <- function(PM, alpha) {
  idx <- lower_index(PM)
  data.frame(
    Comparison = paste(rownames(PM)[idx[, 1L]], rownames(PM)[idx[, 2L]],
                       sep = " vs "),
    p_adjusted = PM[idx],
    Significant = PM[idx] < alpha,
    stringsAsFactors = FALSE
  )
}

run_multi_group <- function(data, paired, parametric, p_adjust, alpha) {
  lv <- levels(data$Group)
  k <- length(lv)
  N <- nrow(data)
  PM <- matrix(NA_real_, k, k, dimnames = list(lv, lv))

  if (paired) {
    n <- N / k
    mat <- matrix(data$Value, ncol = k)
    r <- stats::friedman.test(mat)
    chi <- unname(r$statistic)
    test <- list(name = "Friedman test", stat_name = "chi-squared", stat = chi,
                 df = format(unname(r$parameter)), p = r$p.value,
                 effect_name = "Kendall's W", effect = chi / (n * (k - 1)))
    pw <- suppressWarnings(stats::pairwise.wilcox.test(
      data$Value, data$Group, paired = TRUE,
      p.adjust.method = p_adjust, exact = FALSE))
    PM <- fill_pm(PM, pw$p.value)
    post <- pm_table(PM, alpha)
    method <- sprintf("Pairwise Wilcoxon signed-rank tests (%s adjusted)", p_adjust)

  } else if (parametric) {
    fit <- stats::aov(Value ~ Group, data = data)
    s <- summary(fit)[[1L]]
    ss <- s[["Sum Sq"]]
    test <- list(name = "One-way ANOVA", stat_name = "F",
                 stat = s[["F value"]][1L],
                 df = paste(s[["Df"]][1L], s[["Df"]][2L], sep = ", "),
                 p = s[["Pr(>F)"]][1L],
                 effect_name = "eta-squared", effect = ss[1L] / sum(ss))
    tk <- stats::TukeyHSD(fit)$Group
    idx <- lower_index(PM)
    if (nrow(idx) != nrow(tk)) stop("Unexpected Tukey HSD output.")
    PM[idx] <- tk[, "p adj"]
    PM[idx[, 2:1, drop = FALSE]] <- tk[, "p adj"]
    post <- data.frame(
      Comparison = paste(lv[idx[, 1L]], lv[idx[, 2L]], sep = " vs "),
      Mean_difference = tk[, "diff"], CI_low = tk[, "lwr"],
      CI_high = tk[, "upr"], p_adjusted = tk[, "p adj"],
      Significant = tk[, "p adj"] < alpha, stringsAsFactors = FALSE)
    rownames(post) <- NULL
    method <- "Tukey HSD (difference = first group minus second group)"

  } else {
    r <- stats::kruskal.test(Value ~ Group, data = data)
    h <- unname(r$statistic)
    test <- list(name = "Kruskal-Wallis test", stat_name = "H", stat = h,
                 df = format(unname(r$parameter)), p = r$p.value,
                 effect_name = "epsilon-squared", effect = h / (N - 1))
    pw <- suppressWarnings(stats::pairwise.wilcox.test(
      data$Value, data$Group, p.adjust.method = p_adjust, exact = FALSE))
    PM <- fill_pm(PM, pw$p.value)
    post <- pm_table(PM, alpha)
    method <- sprintf("Pairwise Wilcoxon rank-sum tests (%s adjusted)", p_adjust)
  }
  list(test = test, PM = PM, post = post, post_method = method)
}

# Compact letter display (needs the optional 'multcompView' package).
# Groups sharing a letter are not significantly different; 'a' goes to the
# group with the highest centre (mean or median).
compact_letters <- function(PM, center, alpha) {
  if (!requireNamespace("multcompView", quietly = TRUE)) return(NULL)
  k <- nrow(PM)
  if (anyNA(PM[lower.tri(PM)])) return(NULL)
  lab <- character(k)
  lab[order(-center)] <- sprintf("G%02d", seq_len(k))
  idx <- lower_index(PM)
  p <- PM[idx]
  names(p) <- paste(lab[idx[, 1L]], lab[idx[, 2L]], sep = "-")
  let <- multcompView::multcompLetters(p, threshold = alpha)$Letters
  stats::setNames(unname(let[lab]), rownames(PM))
}

compare_groups <- function(data, paired = FALSE, test = "auto", alpha = 0.05,
                           p_adjust = "holm") {
  test <- match.arg(test, c("auto", "parametric", "nonparametric"))
  notes <- character()

  data <- data[!is.na(data$Value) & !is.na(data$Group), , drop = FALSE]
  data$Group <- droplevels(data$Group)
  k <- nlevels(data$Group)

  result <- function(data, ...) {
    base <- list(data = data, descriptives = describe_groups(data),
                 assumptions = NULL, test = NULL, test_table = NULL,
                 posthoc = NULL, posthoc_method = NULL, letters = NULL,
                 paired = FALSE, parametric = NA, summary_text = NULL,
                 notes = notes)
    utils::modifyList(base, list(...))
  }

  if (k < 2L) {
    notes <- "Only one group contains data, so no comparison was made."
    return(result(data))
  }

  if (paired) {
    blocks <- if ("Subject" %in% names(data)) complete_blocks(data) else NULL
    if (is.null(blocks) || nrow(blocks) == 0L) {
      notes <- c(notes, paste("A paired analysis was requested but the data",
                              "cannot be paired; groups were treated as",
                              "independent."))
      paired <- FALSE
    } else {
      dropped <- length(unique(data$Subject)) - length(unique(blocks$Subject))
      if (dropped > 0L) {
        notes <- c(notes, sprintf(paste("%d subject(s) with missing values were",
                                        "excluded from the paired analysis."),
                                  dropped))
      }
      data <- blocks
      data$Group <- droplevels(data$Group)
    }
  }

  if (any(table(data$Group) < 2L)) {
    notes <- c(notes, "Each group needs at least 2 observations to be tested.")
    return(result(data, paired = paired))
  }

  ass <- check_assumptions(data, paired, alpha)
  parametric <- switch(test,
    parametric = TRUE,
    nonparametric = FALSE,
    auto = ass$normal_ok && ass$var_ok
  )
  if (paired && k > 2L) {
    if (test == "parametric") {
      notes <- c(notes, paste("Parametric repeated-measures ANOVA is not",
                              "implemented; the Friedman test was used."))
    }
    parametric <- FALSE
  }

  if (test == "auto") {
    why <- if (!ass$normal_ok) {
      "normality was rejected or could not be assessed (Shapiro-Wilk)"
    } else if (!ass$var_ok) {
      "equal variances were rejected (Fligner-Killeen)"
    } else {
      "no violation of normality or equal variances was detected"
    }
    notes <- c(notes, sprintf("Test chosen automatically: %s, because %s.",
                              if (parametric) "parametric" else "non-parametric",
                              why))
  } else {
    notes <- c(notes, sprintf("Test type set by user: %s.", test))
  }
  if (any(table(data$Group) > 5000L)) {
    notes <- c(notes, paste("Shapiro-Wilk was run on an evenly spaced",
                            "subsample of 5000 values in large groups."))
  }

  core <- tryCatch(
    if (k == 2L) {
      run_two_groups(data, paired, parametric)
    } else {
      run_multi_group(data, paired, parametric, p_adjust, alpha)
    },
    error = function(e) {
      notes <<- c(notes, paste("The test could not be computed:",
                               conditionMessage(e)))
      NULL
    }
  )

  out <- result(data, paired = paired, parametric = parametric)
  out$notes <- notes
  out$assumptions <- ass
  if (is.null(core)) return(out)

  t <- core$test
  out$test <- t
  out$test_table <- data.frame(
    Test = t$name, Statistic = t$stat_name, Value = t$stat, df = t$df,
    p = t$p, Effect_size_type = t$effect_name, Effect_size = t$effect,
    N = nrow(data), Groups = k, Paired = paired, stringsAsFactors = FALSE)
  out$posthoc <- core$post
  out$posthoc_method <- core$post_method

  if (k > 2L && !is.null(core$PM)) {
    center <- if (parametric) out$descriptives$Mean else out$descriptives$Median
    out$letters <- compact_letters(core$PM, center, alpha)
    if (is.null(out$letters)) {
      out$notes <- c(out$notes, paste("Install the optional package",
                                      "'multcompView' to add significance",
                                      "letters to the plot."))
    }
  }

  df_part <- if (nzchar(t$df)) sprintf("(%s)", t$df) else ""
  out$summary_text <- sprintf("%s: %s%s = %.2f, %s; %s = %.2f", t$name,
                              t$stat_name, df_part, t$stat, p_text(t$p),
                              t$effect_name, t$effect)
  out
}
