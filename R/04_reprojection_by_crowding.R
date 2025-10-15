# =============================================================================
# 04_reprojection_by_crowding.R
# Reprojection-error distributions by crowding condition
# =============================================================================

source(here::here("R", "00_setup.R"))

# Expected input:
#   data/raw/ff1.csv
#
# Required columns:
#   Consition - method/condition label used in the original dataset
#   Threshold - reprojection error in pixels

DATA_FILE <- here::here("data", "raw", "ff1.csv")
df <- read_csv(DATA_FILE, show_col_types = FALSE)

require_columns(df, c("Consition", "Threshold"), "crowding reprojection data")

condition_order <- c(
  "DSAC-Empty", "ESAC4-Empty", "ESAC10-Empty", "HLoc-Empty", "ACE-Empty",
  "DSAC-Semi", "ESAC4-Semi", "ESAC10-Semi", "HLoc-Semi", "ACE-Semi"
)

df <- df %>%
  mutate(
    Consition = factor(Consition, levels = condition_order)
  )

p <- ggplot(df, aes(x = Consition, y = Threshold, fill = Consition)) +
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
    title = "Reprojection error under crowding conditions",
    x = NULL,
    y = "Reprojection error (pixels)"
  ) +
  theme_phd() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)
save_phd_plot(p, "reprojection_error_by_crowding.png", width = 10, height = 5.5)
