# Load the code and provide small test fixtures. Tests run from the repo root (see tests/testthat.R).
suppressPackageStartupMessages({ library(tidyverse); library(lubridate); library(slider) })
root <- normalizePath(file.path("..", ".."), mustWork = FALSE)
if (!file.exists(file.path(root, "config", "default.yml"))) root <- normalizePath(".")
Sys.setenv(SONDE_QC_ROOT = root)
source(file.path(root, "R", "utils.R"))
source(file.path(root, "R", "run.R"))
source_all(root)

example_csv <- file.path(root, "examples", "example_station.csv")

# Run the example station through the pipeline with optional settings
run_example <- function(path = example_csv, config = NULL, strictness = NULL, overrides = character()) {
  cfg <- load_config(config, strictness, overrides)
  read <- read_station_csv(path, cfg)
  grid <- regularize(read$data, cfg$data$step_minutes)
  list(res = run_checks(grid, cfg, progress = FALSE), cfg = cfg, read = read, grid = grid)
}

# Row numbers (in the example file) of the planted problems; one day is 96 readings
planted <- list(
  temp_9999 = 2, ph_277 = 3,
  turbidity_spikes = 5,
  do_flat_hours = 8
)
