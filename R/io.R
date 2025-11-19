# =============================================================================
# io.R - read a data file of unknown layout
# =============================================================================
# Supports CSV, TSV, TXT (comma / tab / semicolon / pipe / whitespace separated),
# XLSX and XLS. The separator is detected automatically. Nothing here depends on
# any particular column name.

`%||%` <- function(a, b) if (is.null(a)) b else a

NA_STRINGS <- c("", "NA", "N/A", "n/a", "na", "NaN", "NULL", "null")

# Guess the field separator from the first lines of a text file.
# Returns one of "," "\t" ";" "|" "" (whitespace) or "\n" (single column).
detect_delimiter <- function(path) {
  lines <- readLines(path, n = 20L, warn = FALSE)
  lines <- iconv(lines, "UTF-8", "UTF-8", sub = "?")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0L) stop("The file is empty: ", path, call. = FALSE)

  candidates <- c(",", "\t", ";", "|")
  scores <- vapply(candidates, function(d) {
    counts <- lengths(regmatches(lines, gregexpr(d, lines, fixed = TRUE)))
    if (counts[1L] > 0L && mean(counts == counts[1L]) >= 0.8) {
      as.numeric(counts[1L])
    } else {
      0
    }
  }, numeric(1))

  if (max(scores) > 0) return(candidates[which.max(scores)])

  header_tokens <- strsplit(trimws(lines[1L]), "[[:space:]]+")[[1L]]
  if (length(header_tokens) == 1L) "\n" else ""
}

read_delimited <- function(path, sep) {
  if (identical(sep, "\n")) {
    # Single-column file: every line is one value (values may contain spaces).
    lines <- readLines(path, warn = FALSE)
    lines <- trimws(gsub("\ufeff", "", lines, fixed = TRUE))
    lines <- gsub("^\"|\"$", "", lines[nzchar(lines)])
    values <- lines[-1L]
    values[values %in% NA_STRINGS] <- NA
    df <- data.frame(utils::type.convert(values, as.is = TRUE),
                     stringsAsFactors = FALSE)
    names(df) <- lines[1L]
    return(df)
  }

  args <- list(
    file = path, header = TRUE, sep = sep, quote = "\"", comment.char = "",
    stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE,
    fill = TRUE, na.strings = NA_STRINGS
  )
  suppressWarnings(
    tryCatch(
      do.call(utils::read.table, c(args, list(fileEncoding = "UTF-8-BOM"))),
      error = function(e) do.call(utils::read.table, args)
    )
  )
}

# Text columns such as "12,5" (decimal comma) or "45%" become numeric.
coerce_numeric_like <- function(x) {
  v <- x[!is.na(x)]
  if (length(v) == 0L) return(x)
  v2 <- sub(",", ".", gsub("%$", "", trimws(v)), fixed = TRUE)
  ok <- grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", v2)
  if (!all(ok)) return(x)
  out <- rep(NA_real_, length(x))
  out[!is.na(x)] <- as.numeric(v2)
  out
}

clean_data <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  keep_cols <- vapply(df, function(x) !all(is.na(x)), logical(1))
  df <- df[, keep_cols, drop = FALSE]
  if (ncol(df) > 0L && nrow(df) > 0L) {
    df <- df[rowSums(!is.na(df)) > 0L, , drop = FALSE]
  }

  nm <- trimws(names(df))
  blank <- !nzchar(nm)
  nm[blank] <- paste0("column_", which(blank))
  names(df) <- make.unique(nm)

  for (j in seq_along(df)) {
    if (is.character(df[[j]])) df[[j]] <- coerce_numeric_like(df[[j]])
  }
  rownames(df) <- NULL
  df
}

read_any_file <- function(path, sheet = 1L) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading Excel files needs the 'readxl' package: ",
           "install.packages(\"readxl\")", call. = FALSE)
    }
    df <- as.data.frame(readxl::read_excel(path, sheet = sheet),
                        stringsAsFactors = FALSE)
  } else if (ext %in% c("csv", "tsv", "txt", "dat", "tab", "")) {
    df <- read_delimited(path, detect_delimiter(path))
  } else {
    stop("Unsupported file type '.", ext,
         "'. Supported: csv, tsv, txt, xlsx, xls.", call. = FALSE)
  }
  clean_data(df)
}
