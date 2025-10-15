# =============================================================================
# 01_nasa_workload.R
# NASA-TLX / overall workload analysis
# =============================================================================

source(here::here("R", "00_setup.R"))

# Expected input:
#   data/raw/NASA_TLX_ALL.csv
#
# Required columns:
#   Registration - experimental/registration condition
#   WorkLoad    - overall NASA-TLX workload score

DATA_FILE <- here::here("data", "raw", "NASA_TLX_ALL.csv")
df <- read_csv(DATA_FILE, show_col_types = FALSE)

require_columns(df, c("Registration", "WorkLoad"), "NASA-TLX data")

df <- df %>%
  mutate(Registration = factor(Registration))

p <- ggplot(df, aes(x = Registration, y = WorkLoad, fill = Registration)) +
  geom_boxplot(width = 0.65, alpha = 0.75, outlier.alpha = 0.35) +
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
    aes(label = round(after_stat(y), 2)),
    vjust = -0.8,
    show.legend = FALSE
  ) +
  labs(
    title = "Overall NASA-TLX workload",
    x = NULL,
    y = "Overall workload"
  ) +
  theme_phd() +
  theme(legend.position = "none")

print(p)
save_phd_plot(p, "nasa_overall_workload.png")
