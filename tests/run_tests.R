# =============================================================================
# tests/run_tests.R - smoke tests using the synthetic files in examples/
# =============================================================================
# Run from the project root:   Rscript tests/run_tests.R
# Every line should say PASS. Please report any FAIL together with its message.

root <- normalizePath(getwd())
source(file.path(root, "R", "load.R"))
load_project(root)

failures <- 0L
check <- function(name, expr) {
  ok <- tryCatch(isTRUE(expr), error = function(e) {
    message("    error: ", conditionMessage(e))
    FALSE
  })
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", name))
  if (!ok) failures <<- failures + 1L
  invisible(ok)
}
ex <- function(f) file.path(root, "examples", f)

cat("\n-- Structure detection --\n")
p <- detect_structure(read_any_file(ex("synthetic_long_3groups.csv")))
check("long file: mode is groups/long", p$mode == "groups" && p$layout == "long")
check("long file: group column detected", p$group == "Method")
check("long file: first outcome detected", p$outcomes[1L] == "Score")
check("long file: ID column ignored as outcome", !("Participant_ID" %in% p$outcomes))

p <- detect_structure(read_any_file(ex("synthetic_wide_paired.csv")))
check("wide file: layout is wide with 2 columns",
      p$mode == "groups" && p$layout == "wide" && length(p$value_cols) == 2L)

p <- detect_structure(read_any_file(ex("synthetic_summary_table.csv")))
check("summary table detected", p$mode == "summary" && p$value == "Percentage")

df <- read_any_file(ex("synthetic_semicolon_decimal_comma.csv"))
check("semicolon + decimal comma read as numeric", is.numeric(df$Value))
p <- detect_structure(df)
check("semicolon file: group/outcome detected",
      p$group == "Group" && p$outcomes[1L] == "Value")

p <- detect_structure(read_any_file(ex("synthetic_categorical_only.csv")))
check("categorical-only file: frequency mode",
      p$mode == "frequency" && p$category == "Response")

p <- detect_structure(read_any_file(ex("synthetic_repeated_measures.csv")))
check("repeated measures: subject column found, not used as outcome",
      p$group == "Time_point" && p$outcomes[1L] == "Score" && p$id == "Subject")

cat("\n-- Full analyses (output goes to a temporary folder) --\n")
run <- function(file, ...) {
  out <- tempfile("analysis_")
  res <- suppressMessages(capture.output(
    r <- analyse(ex(file), outdir = out, confirm = FALSE, ...)))
  list(res = r, out = out)
}
has <- function(out, pattern) length(list.files(out, pattern = pattern)) > 0L

r <- run("synthetic_long_3groups.csv")
check("3 groups: figure + report written",
      has(r$out, "groups_Score\\.png$") && has(r$out, "groups_Score_report\\.txt$"))
check("3 groups: post hoc table written", has(r$out, "groups_Score_posthoc\\.csv$"))

r <- run("synthetic_long_3groups.csv", outcome = "all")
check("outcome = 'all' analyses both numeric columns",
      has(r$out, "groups_Score\\.png$") && has(r$out, "groups_Time_s\\.png$"))

r <- run("synthetic_wide_paired.csv")
check("wide paired: test table says paired", {
  tt <- read.csv(file.path(r$out, "groups_comparison_test.csv"))
  isTRUE(tt$Paired[1L])
})

r <- run("synthetic_wide_paired.csv", log_y = TRUE)
check("log scale runs", has(r$out, "groups_comparison\\.png$"))

r <- run("synthetic_repeated_measures.csv")
check("repeated measures: Friedman test used", {
  tt <- read.csv(file.path(r$out, "groups_Score_test.csv"))
  tt$Test[1L] == "Friedman test"
})

r <- run("synthetic_semicolon_decimal_comma.csv", test = "nonparametric")
check("2 independent groups, non-parametric", {
  tt <- read.csv(file.path(r$out, "groups_Value_test.csv"))
  grepl("Mann-Whitney", tt$Test[1L])
})

r <- run("synthetic_summary_table.csv")
check("summary table: bar chart written", has(r$out, "bars_Percentage\\.png$"))

r <- run("synthetic_categorical_only.csv")
check("categorical only: frequency chart written", has(r$out, "frequency_Response\\.png$"))

cat("\n-- Helpful errors --\n")
check("misspelled column gives a clear error", {
  msg <- tryCatch(
    analyse(ex("synthetic_long_3groups.csv"), group = "Metod",
            outdir = tempfile(), confirm = FALSE),
    error = function(e) conditionMessage(e))
  grepl("did you mean 'Method'", msg, fixed = TRUE)
})

cat(sprintf("\n%s\n", if (failures == 0L) "All tests passed." else
  sprintf("%d test(s) FAILED.", failures)))
if (!interactive() && failures > 0L) quit(status = 1L)
