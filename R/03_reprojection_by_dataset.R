# =============================================================================
# 03_reprojection_by_dataset.R
# Reprojection-error distributions for the individual environment datasets
# =============================================================================

source(here::here("R", "00_setup.R"))

# Expected input:
#   data/raw/nn.csv
#
# Required columns:
#   Methods   - localisation / pose-estimation method
#   Threshold9 - reprojection error in pixels
#
# Update DATA_FILE if nn.csv is stored under a different authorised filename.

DATA_FILE <- here::here("data", "raw", "nn.csv")
df <- read_csv(DATA_FILE, show_col_types = FALSE)

require_columns(df, c("Methods", "Threshold9"), "reprojection data")

method_order <- c("DSAC++", "ESAC- 4", "ESAC-10", "H-Loc", "ACE")

df <- df %>%
  mutate(
    Methods = factor(Methods, levels = method_order)
  )

p <- ggplot(df, aes(x = Methods, y = Threshold9, fill = Methods)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75, width = 0.65) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 0.8) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 3,
    fill = "white",
    colour = "black"
  ) +
  labs(
    title = "Reprojection error by method",
    x = "Method",
    y = "Reprojection error (pixels)"
  ) +
  theme_phd() +
  theme(legend.position = "none")

print(p)
save_phd_plot(p, "reprojection_error_by_method.png")
