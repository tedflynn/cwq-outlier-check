#!/usr/bin/env Rscript
# Outlier checks for one fixed station. Usage:
#   Rscript run_qc.R data.csv [--config my_settings.yml] [--strictness 1.5] [--set turbidity.spike.min_dev=10]
#                            [--out output/folder] [--name label] [--no-report]
# --set can be repeated. See README.md and config/default.yml.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0 || args[1] %in% c("-h", "--help")) {
  cat("Usage: Rscript run_qc.R data.csv [--config file.yml] [--strictness x] [--set path=value] [--out dir] [--name label] [--no-report]\n")
  quit(status = 0)
}
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
Sys.setenv(SONDE_QC_ROOT = normalizePath(script_dir))
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(slider) })
source(file.path(script_dir, "R", "utils.R"))
source(file.path(script_dir, "R", "run.R"))
source_all()

opt <- list(config = NULL, strictness = NULL, out = NULL, name = NULL, report = TRUE, set = character())
input <- args[1]; i <- 2
while (i <= length(args)) {
  a <- args[i]
  if (a == "--no-report") { opt$report <- FALSE; i <- i + 1; next }
  val <- args[i + 1]
  switch(a, "--config" = opt$config <- val, "--strictness" = opt$strictness <- as.numeric(val), "--out" = opt$out <- val,
         "--name" = opt$name <- val, "--set" = opt$set <- c(opt$set, val), stop("Unknown option ", a, call. = FALSE))
  i <- i + 2
}
run_qc(input, config = opt$config, strictness = opt$strictness, overrides = opt$set, out_dir = opt$out,
       name = opt$name, report = opt$report)
