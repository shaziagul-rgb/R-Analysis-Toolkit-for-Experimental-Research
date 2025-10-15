# =============================================================================
# 02_mixed_reality.R
# Mixed-reality registration / plausibility analysis
# =============================================================================

source(here::here("R", "00_setup.R"))

# Expected input:
#   data/raw/MR_all.txt
#
# Required columns:
#   Reg_quest - questionnaire item / rating
#   value     - rating value
#   Registration - registration condition (if available)

DATA_FILE <- here::here("data", "raw", "MR_all.txt")
df <- read_table(DATA_FILE, show_col_types = FALSE)

require_columns(df, c("Reg_quest", "value"), "mixed-reality data")

df <- df %>%
  mutate(Reg_quest = factor(Reg_quest))

# Tukey HSD grouping is retained from the original PhD analysis.
# It is only calculated when agricolae is available.
if (requireNamespace("agricolae", quietly = TRUE)) {
  model <- aov(value ~ Reg_quest, data = df)
  hsd <- agricolae::HSD.test(model, trt = "Reg_quest", group = TRUE)

  groups <- hsd$groups %>%
    tibble::rownames_to_column("Reg_quest") %>%
    select(Reg_quest, groups)

  label_data <- df %>%
    group_by(Reg_quest) %>%
    summarise(y = max(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(groups, by = "Reg_quest") %>%
    mutate(y = y * 1.05)
} else {
  label_data <- NULL
  message("Package 'agricolae' not installed; Tukey group letters omitted.")
}

p <- ggplot(df, aes(x = Reg_quest, y = value)) +
  geom_boxplot(aes(fill = Reg_quest), width = 0.65, alpha = 0.75) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3,
    fill = "white",
    colour = "black"
  ) +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = paste0("Mean = ", round(after_stat(y), 2))),
    vjust = -1.0,
    size = 3.2,
    show.legend = FALSE
  ) +
  labs(
    title = "Mixed-reality ratings",
    x = NULL,
    y = "Mixed-reality rating"
  ) +
  theme_phd() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

if (!is.null(label_data)) {
  p <- p +
    geom_text(
      data = label_data,
      aes(x = Reg_quest, y = y, label = groups),
      inherit.aes = FALSE,
      fontface = "bold"
    )
}

print(p)
save_phd_plot(p, "mixed_reality_ratings.png", width = 9, height = 5.5)
