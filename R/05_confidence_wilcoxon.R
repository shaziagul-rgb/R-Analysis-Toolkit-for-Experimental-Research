# =============================================================================
# 05_confidence_wilcoxon.R
# Paired confidence comparison: Manual vs ESAC
# =============================================================================

source(here::here("R", "00_setup.R"))

# Expected input:
#   data/raw/confidence.csv
#
# Required columns:
#   Manual
#   ESAC

DATA_FILE <- here::here("data", "raw", "confidence.csv")
df <- read_csv(DATA_FILE, show_col_types = FALSE)

require_columns(df, c("Manual", "ESAC"), "confidence data")

# Normality checks retained from the original analysis.
shapiro_manual <- shapiro.test(df$Manual)
shapiro_esac <- shapiro.test(df$ESAC)

# Paired non-parametric comparison.
wilcox_result <- wilcox.test(
  df$Manual,
  df$ESAC,
  paired = TRUE,
  exact = FALSE
)

print(shapiro_manual)
print(shapiro_esac)
print(wilcox_result)

# Save the test output as plain text for the research record.
sink(here::here("results", "confidence_wilcoxon.txt"))
cat("Confidence analysis: Manual vs ESAC\n\n")
cat("Shapiro-Wilk test: Manual\n")
print(shapiro_manual)
cat("\nShapiro-Wilk test: ESAC\n")
print(shapiro_esac)
cat("\nPaired Wilcoxon signed-rank test\n")
print(wilcox_result)
sink()
