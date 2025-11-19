# =============================================================================
# run.R - start here
# =============================================================================
# In RStudio:      source("run.R")          (a file chooser opens)
# From a terminal: Rscript run.R my_data.csv [options]
#
# Options (all optional - columns are detected automatically):
#   --group COL      grouping / condition column
#   --outcome COL    numeric outcome column (or "all")
#   --id COL         subject column (enables paired analysis)
#   --columns A,B,C  columns that are each one condition (wide layout)
#   --paired true|false
#   --test auto|parametric|nonparametric
#   --outdir DIR     where to save results (default: outputs/<file name>/)
#   --title TEXT     --xlab TEXT     --ylab TEXT
#   --log-y          log scale on the y axis
#   --help

script_root <- function() {
  args <- commandArgs(FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  root <- if (length(f) > 0L) dirname(normalizePath(f[1L])) else getwd()
  if (!file.exists(file.path(root, "R", "load.R"))) {
    stop("Cannot find the project folder. Open R-Analysis-Toolkit-for-Experimental-Research.Rproj in RStudio ",
         "or setwd() to the project folder first.", call. = FALSE)
  }
  root
}

root <- script_root()
source(file.path(root, "R", "load.R"))
load_project(root)

parse_cli <- function(args) {
  opts <- list()
  positional <- character()
  i <- 1L
  flags <- c("log-y", "help")
  while (i <= length(args)) {
    a <- args[i]
    if (startsWith(a, "--")) {
      key <- sub("^--", "", a)
      if (key %in% flags) {
        opts[[key]] <- TRUE
        i <- i + 1L
      } else {
        if (i == length(args)) stop("Option --", key, " needs a value.", call. = FALSE)
        opts[[key]] <- args[i + 1L]
        i <- i + 2L
      }
    } else {
      positional <- c(positional, a)
      i <- i + 1L
    }
  }
  list(opts = opts, path = if (length(positional) > 0L) positional[1L] else NULL)
}

if (!interactive() && sys.nframe() == 0L) {
  cli <- parse_cli(commandArgs(trailingOnly = TRUE))
  o <- cli$opts
  if (isTRUE(o$help) || is.null(cli$path)) {
    h <- readLines(file.path(root, "run.R"))
    h <- h[seq_len(which(!startsWith(h, "#"))[1L] - 1L)]
    cat(paste(sub("^# ?", "", h[-(1:2)]), collapse = "\n"), "\n")
  } else {
    tf <- function(x) if (is.null(x)) NULL else tolower(x) %in% c("true", "t", "yes", "1")
    analyse(
      cli$path, group = o$group, outcome = o$outcome, id = o$id,
      columns = if (is.null(o$columns)) NULL else strsplit(o$columns, ",")[[1L]],
      paired = tf(o$paired), test = o$test %||% "auto", outdir = o$outdir,
      log_y = isTRUE(o[["log-y"]]), title = o$title, xlab = o$xlab, ylab = o$ylab,
      confirm = FALSE
    )
  }
} else if (interactive()) {
  message("Project loaded. Starting analysis - choose your data file.\n",
          "(To run again later: analyse(); see ?analyse for options.)")
  results <- analyse()
}
