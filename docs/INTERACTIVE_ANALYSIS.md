# Interactive analysis

`R/interactive_analysis.R` is intentionally independent of the original PhD
column names. It is designed as a reusable starting point for researchers.

The workflow accepts CSV/TXT/TSV/Excel files and asks the user to select:

- a grouping/condition variable
- a numeric outcome variable

It then creates:

- group-level N, mean, SD, median, minimum and maximum
- a boxplot with group means
- a CSV summary table

For specialised analyses, add separate scripts rather than hiding statistical
assumptions inside a generic tool.
