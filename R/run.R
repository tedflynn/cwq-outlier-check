# The whole job: read a CSV, run the checks, write the tables, render the Quarto report.

source_all <- function(root = qc_root()) {
  for (f in c("config.R", "read_data.R", "checks.R", "pipeline.R", "integrity.R", "summary.R", "plots.R"))
    source(file.path(root, "R", f), local = FALSE)
}

# Find the Quarto program: on the PATH, in QUARTO_PATH, or in the usual install and RStudio locations
find_quarto <- function() {
  cands <- c(unname(Sys.which("quarto")), Sys.getenv("QUARTO_PATH"), "/Applications/quarto/bin/quarto", "/usr/local/bin/quarto",
             "/opt/homebrew/bin/quarto", "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto")
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (length(cands)) cands[[1]] else NULL
}

# Check one station's CSV.
#   input       path to the CSV
#   config      path to a settings file with the lines you want to change (default: config/default.yml only)
#   strictness  one number that scales sensitivity (see config/default.yml); overrides the file
#   overrides   single settings as "path=value", e.g. "turbidity.spike.min_dev=10"
#   out_dir     where to write (default output/<name>); name defaults to the file name
#   report      render the Quarto report (needs Quarto)
# Returns the results invisibly: res$data (flagged readings), res$summary, res$outcomes, res$checks, res$events,
# res$file_checks, and the settings used.
run_qc <- function(input, config = NULL, strictness = NULL, overrides = character(), out_dir = NULL,
                   name = NULL, report = TRUE) {
  root <- qc_root()
  name <- name %||% tools::file_path_sans_ext(basename(input))
  out_dir <- out_dir %||% file.path(root, "output", name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  cfg <- load_config(config, strictness, overrides)
  message("Reading ", input)
  read <- read_station_csv(input, cfg)
  grid <- regularize(read$data, cfg$data$step_minutes)
  found <- names(read$columns)[!is.na(read$columns)]
  message("  ", format(read$rows_read, big.mark = ","), " rows; parameters found: ", paste(found, collapse = ", "))

  message("Running checks (strictness ", cfg$strictness, ")")
  res <- run_checks(grid, cfg)
  res$name <- name
  res$input <- normalizePath(input)
  res$columns <- read$columns
  res$summary <- summary_table(res)
  res$outcomes <- outcome_table(res)
  res$check_table <- check_table(res)
  res$events <- events_table(res)
  res$file_checks <- file_checks(read, grid, cfg)
  res$settings <- settings_table(cfg, res$params)

  fmt_time <- function(d) { d$time <- format(d$time, "%Y-%m-%d %H:%M:%S"); d }
  readr::write_csv(fmt_time(res$data), file.path(out_dir, paste0(name, "_flagged.csv")), na = "")
  readr::write_csv(res$summary, file.path(out_dir, paste0(name, "_summary.csv")))
  readr::write_csv(res$outcomes, file.path(out_dir, paste0(name, "_outcomes.csv")))
  readr::write_csv(res$check_table, file.path(out_dir, paste0(name, "_checks.csv")))
  readr::write_csv(res$events, file.path(out_dir, paste0(name, "_events.csv")))
  readr::write_csv(res$file_checks, file.path(out_dir, paste0(name, "_file_checks.csv")))
  yaml::write_yaml(cfg, file.path(out_dir, paste0(name, "_settings.yml")))
  rds <- file.path(out_dir, paste0(name, "_results.rds"))
  saveRDS(res, rds)

  print_summary(res)
  if (report) render_report(rds, out_dir, name, root)
  invisible(res)
}

print_summary <- function(res) {
  cat("\nWhat the checks found (readings with data; % of those):\n")
  s <- res$summary
  print(as.data.frame(dplyr::transmute(s, parameter = label, readings = with_data, passed = passed,
                                       removed = removed, `% removed` = round(pct_removed, 2),
                                       `flagged for review` = flagged_for_review, `% flagged` = round(pct_flagged, 2))), row.names = FALSE)
  bad <- res$file_checks[res$file_checks$status == "check", ]
  if (nrow(bad)) { cat("\nFile checks that need a look:\n"); print(as.data.frame(bad[, c("check", "parameter", "value")]), row.names = FALSE) }
}

render_report <- function(rds, out_dir, name, root = qc_root()) {
  q <- find_quarto()
  if (is.null(q)) {
    message("Quarto was not found, so the report was not rendered. The tables are in ", out_dir,
            ". Install Quarto (quarto.org) and run:\n  quarto render qc-report.qmd -P results:", rds)
    return(invisible(NULL))
  }
  message("Rendering the report")
  html <- file.path(root, "qc-report.html")
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  status <- system2(q, c("render", "qc-report.qmd", "-P", paste0("results:", rds), "--quiet"))
  if (status != 0 || !file.exists(html)) { message("The report did not render (Quarto exit status ", status, ")."); return(invisible(NULL)) }
  target <- file.path(out_dir, paste0(name, "_report.html"))
  file.copy(html, target, overwrite = TRUE); file.remove(html)
  message("Report: ", target)
  invisible(target)
}
