# =============================================================================
# 06_participant_experience.R
# Participant AR/VR experience distribution
# =============================================================================

# These percentages were explicitly recorded in the original PhD script.
# They are retained here as an archival summary because the participant-level
# source sheet is no longer available.

source(here::here("R", "00_setup.R"))

experience <- data.frame(
  Experience = c(
    "No experience",
    "AR experience",
    "VR experience",
    "AR/VR experience"
  ),
  Percentage = c(16.6, 4.1, 37.5, 41.6)
)

write_csv(
  experience,
  here::here("data", "processed", "participant_experience_archived.csv")
)

p <- ggplot(
  experience,
  aes(x = reorder(Experience, Percentage), y = Percentage)
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    hjust = -0.15
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 50),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Participant AR/VR experience",
    x = NULL,
    y = "Participants (%)"
  ) +
  theme_phd() +
  theme(legend.position = "none")

print(p)
save_phd_plot(p, "participant_ar_vr_experience.png", width = 7, height = 4.5)
