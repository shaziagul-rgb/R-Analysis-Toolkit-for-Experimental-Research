# =============================================================================
# 07_response_distribution.R
# Archived response distribution from the original PhD script
# =============================================================================

source(here::here("R", "00_setup.R"))

# The original script recorded these percentages directly.
# This is an archival reconstruction, not a reconstruction of raw responses.

response <- data.frame(
  Response = c("Positive", "No comments"),
  Percentage = c(90, 10)
)

write_csv(
  response,
  here::here("data", "processed", "response_distribution_archived.csv")
)

p <- ggplot(response, aes(x = Response, y = Percentage)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    vjust = -0.4
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Recorded response distribution",
    x = NULL,
    y = "Responses (%)"
  ) +
  theme_phd() +
  theme(legend.position = "none")

print(p)
save_phd_plot(p, "response_distribution_archived.png", width = 6, height = 4.5)
