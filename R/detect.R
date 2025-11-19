# =============================================================================
# detect.R - work out the structure of a dataset automatically
# =============================================================================
# detect_structure() inspects the columns and decides what kind of analysis the
# data support. It never relies on specific column names; names are only used
# as weak hints to break ties (e.g. a column called "Score" is preferred as an
# outcome over an anonymous numeric column).

ID_PATTERN   <- "(^|[^a-z])(id|ids|pid|subject|subj|participant|respondent|index|idx|row|trial)([^a-z]|$)"
GROUP_HINT   <- "group|condition|cond|method|treat|arm|type|class|categor|factor|registration|level|season|dataset|model|algorithm|device|cohort|variant|setting"
OUTCOME_HINT <- "score|value|outcome|error|rating|response|time|load|measure|result|threshold|accuracy|confidence|percent|mean|total|duration"

profile_columns <- function(df) {
  rows <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    present <- x[!is.na(x)]
    kind <- if (is.numeric(x)) {
      "numeric"
    } else if (is.character(x) || is.factor(x) || is.logical(x)) {
      "categorical"
    } else {
      "other"
    }
    low <- tolower(nm)
    data.frame(
      column = nm,
      kind = kind,
      n_unique = length(unique(present)),
      n_nonmissing = length(present),
      n_missing = sum(is.na(x)),
      id_like = grepl(ID_PATTERN, low) && !grepl(GROUP_HINT, low),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Columns that look like a grouping / condition variable, best guess first.
group_candidates <- function(prof, n) {
  hint <- grepl(GROUP_HINT, tolower(prof$column))
  cat_ok <- prof$kind == "categorical" & prof$n_unique >= 2L &
    prof$n_unique <= min(30, floor(n / 2)) & !prof$id_like
  int_ok <- prof$kind == "numeric" & prof$n_unique >= 2L &
    prof$n_unique <= 6L & hint & !prof$id_like
  keep <- cat_ok | int_ok
  if (!any(keep)) return(character())
  p <- prof[keep, , drop = FALSE]
  score <- 3 * hint[keep] + 2 * (p$n_unique <= 10L)
  p$column[order(-score)]
}

# Numeric columns that could be the measured outcome, best guess first.
outcome_candidates <- function(prof, exclude = character()) {
  keep <- prof$kind == "numeric" & !prof$id_like & prof$n_unique >= 2L &
    !(prof$column %in% exclude)
  p <- prof[keep, , drop = FALSE]
  if (nrow(p) == 0L) return(character())
  score <- 3 * grepl(OUTCOME_HINT, tolower(p$column)) + log10(p$n_unique)
  p$column[order(-score)]
}

# Returns a "plan": a list describing the analysis mode and chosen columns.
#   mode = "groups"       compare a numeric outcome between groups
#            layout = "long": one grouping column + numeric outcome column(s)
#            layout = "wide": each numeric column is one group / condition
#   mode = "summary"      a ready-made table of labels + values (e.g. percentages)
#   mode = "frequency"    only categorical data: count the categories
#   mode = "distribution" a single numeric column
detect_structure <- function(df) {
  n <- nrow(df)
  prof <- profile_columns(df)
  notes <- character()

  numeric_cols <- prof$column[prof$kind == "numeric" & !prof$id_like]
  cat_prof <- prof[prof$kind == "categorical", , drop = FALSE]

  # 1. No numbers at all: count categories.
  if (length(numeric_cols) == 0L) {
    ok <- cat_prof[!cat_prof$id_like, , drop = FALSE]
    if (nrow(ok) == 0L) {
      stop("No usable numeric or categorical columns were found.", call. = FALSE)
    }
    small <- ok[ok$n_unique <= 30L, , drop = FALSE]
    pick <- if (nrow(small) > 0L) small$column[1L] else ok$column[1L]
    return(list(mode = "frequency", category = pick, profile = prof,
                notes = notes))
  }

  # 2. A ready-made table: one label column (all different) + one number column.
  all_unique <- cat_prof$column[cat_prof$n_unique == cat_prof$n_nonmissing &
                                  cat_prof$n_unique >= 2L]
  if (nrow(cat_prof) == 1L && length(all_unique) == 1L &&
      length(numeric_cols) == 1L && n <= 40L) {
    return(list(mode = "summary", label = all_unique, value = numeric_cols,
                profile = prof, notes = notes))
  }

  # 3. A grouping column plus numeric outcome(s): "long" layout.
  gc <- group_candidates(prof, n)
  id_cols <- prof$column[prof$id_like]
  if (length(gc) > 0L) {
    group <- gc[1L]
    outcomes <- outcome_candidates(prof, exclude = group)
    if (length(outcomes) == 0L) {
      stop("A grouping column ('", group,
           "') was found, but there is no numeric column to analyse.",
           call. = FALSE)
    }
    if (length(gc) > 1L) {
      notes <- c(notes, paste0("Other possible grouping columns: ",
                               paste(gc[-1L], collapse = ", ")))
    }
    return(list(mode = "groups", layout = "long", group = group,
                outcomes = outcomes,
                id = if (length(id_cols) > 0L) id_cols[1L] else NULL,
                profile = prof, notes = notes))
  }

  # 4. No grouping column but several numeric columns: "wide" layout.
  if (length(numeric_cols) >= 2L) {
    notes <- c(notes, paste0(
      "No category column found, so each numeric column is treated as one ",
      "group/condition. If these are different variables rather than ",
      "conditions, choose columns yourself (see ?analyse)."))
    return(list(mode = "groups", layout = "wide", value_cols = numeric_cols,
                profile = prof, notes = notes))
  }

  # 5. One numeric column.
  list(mode = "distribution", column = numeric_cols[1L], profile = prof,
       notes = notes)
}
