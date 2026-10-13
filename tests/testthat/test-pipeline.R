test_that("the example station: planted range problems are removed", {
  r <- run_example()$res
  expect_equal(sum(r$data$water_temp_flag == "Removed: Range"), 2)
  expect_equal(sum(r$data$ph_flag == "Removed: Range"), 3)
  expect_true(all(is.na(r$data$water_temp_clean[r$data$water_temp_flag == "Removed: Range"])))
  expect_false(any(r$data$water_temp_clean > 100, na.rm = TRUE))
})

test_that("the example station: spikes removed, real event kept, fouling flagged, erratic stretch flagged", {
  r <- run_example()$res
  d <- r$data
  expect_gte(sum(d$turbidity_flag == "Removed: Isolated spike"), 5)
  expect_gt(sum(d$turbidity_flag == "Kept: Corroborated event"), 0)                     # the storm, matched by conductivity and DO
  expect_gt(sum(d$turbidity_flag == "Flagged: Uncorroborated excursion (review)"), 0)   # the fouling
  expect_gt(sum(d$turbidity_flag == "Flagged: Erratic period (review)"), 40)
  storm_day <- as_date(min(d$time)) + 30
  expect_true(any(d$turbidity_flag[as_date(d$time) == storm_day] == "Kept: Corroborated event"))
  # removed readings are missing in the clean column; flagged ones are kept
  expect_true(all(is.na(d$turbidity_clean[startsWith(d$turbidity_flag, "Removed")])))
  expect_true(all(!is.na(d$turbidity_clean[startsWith(d$turbidity_flag, "Flagged") & !is.na(d$turbidity)])))
})

test_that("the example station: the stuck DO sensor is flagged as a flat line with padding", {
  d <- run_example()$res$data
  flat <- sum(d$dissolved_oxygen_flag == "Flagged: Flat line (review)")
  expect_equal(flat, 8 * 4 + 2 * 24)                 # 8 h of readings, 6 h either side
})

test_that("clean sensors stay clean: no flags on conductivity or chlorophyll", {
  d <- run_example()$res$data
  expect_equal(sum(d$spec_cond_flag %in% c("Valid", "No data")), nrow(d))
  expect_equal(sum(!d$chlorophyll_flag %in% c("Valid", "No data")), 0)
})

test_that("missing readings are 'No data', not 'Valid'", {
  d <- run_example()$res$data
  expect_true(all(d$chlorophyll_flag[is.na(d$chlorophyll)] == "No data"))
  expect_true(all(!is.na(d$chlorophyll[d$chlorophyll_flag == "Valid"])))
})

test_that("higher strictness never flags fewer readings", {
  flagged <- function(s) { r <- run_example(strictness = s)$res; sum(!r$data$turbidity_flag %in% c("Valid", "No data", "Kept: Corroborated event")) }
  expect_lte(flagged(0.5), flagged(1)); expect_lte(flagged(1), flagged(2))
  expect_lt(flagged(0.5), flagged(3))
})

test_that("actions change what happens: flag instead of remove, and off", {
  base <- run_example()$res$data
  as_flag <- run_example(overrides = "defaults.spike.action_isolated=flag")$res$data
  expect_gt(sum(base$turbidity_flag == "Removed: Isolated spike"), 0)
  expect_equal(sum(as_flag$turbidity_flag == "Removed: Isolated spike"), 0)
  expect_gt(sum(as_flag$turbidity_flag == "Flagged: Isolated spike (review)"), 0)
  off <- run_example(overrides = "defaults.erratic.action=off")$res$data
  expect_equal(sum(off$turbidity_flag == "Flagged: Erratic period (review)"), 0)
})

test_that("the summary tables add up", {
  x <- run_example(); r <- x$res; r$cfg <- x$cfg
  s <- summary_table(r)
  expect_equal(s$passed + s$removed + s$flagged_for_review, s$with_data)
  o <- outcome_table(r)
  expect_equal(sum(o$n[o$parameter == "turbidity"]), s$with_data[s$parameter == "turbidity"])
})

test_that("column names and date formats are detected in other layouts", {
  d <- read_csv(example_csv, show_col_types = FALSE)
  # C-EMP style: separate date and time columns, other column names
  alt <- tibble(station = "X", date = format(mdy_hms(d$Date_Time), "%Y-%m-%d"), time = format(mdy_hms(d$Date_Time), "%H:%M:%S"),
                dissolvedoxygen = d$DO, fluorescence = d$Chlorophyll, ph = d$pH, spc = d$Specific_Conductance,
                turbidity = d$Turbidity, watertemperature = d$Water_Temperature)
  f <- tempfile(fileext = ".csv"); write_csv(alt, f)
  x <- run_example(f)
  expect_equal(sort(x$res$params), sort(parameter_names))
  expect_equal(nrow(x$res$data), nrow(d))
})

test_that("a file with only some parameters runs and skips the rest", {
  d <- read_csv(example_csv, show_col_types = FALSE)
  f <- tempfile(fileext = ".csv"); write_csv(select(d, Date_Time, Turbidity, Water_Temperature), f)
  x <- run_example(f)
  expect_setequal(x$res$params, c("turbidity", "water_temp"))
  fc <- file_checks(x$read, x$grid, x$cfg)
  expect_true(any(fc$check == "Parameters not found in the file" & grepl("ph", fc$value)))
})

test_that("a sorted column is reported by the file checks", {
  d <- read_csv(example_csv, show_col_types = FALSE)
  d$Specific_Conductance <- sort(d$Specific_Conductance, na.last = TRUE)
  f <- tempfile(fileext = ".csv"); write_csv(d, f)
  x <- run_example(f)
  fc <- file_checks(x$read, x$grid, x$cfg)
  ramp <- fc[fc$check == "Steady ramp with time (Spearman rho)" & fc$parameter == "spec_cond", ]
  expect_equal(ramp$status, "check")
  other <- fc[fc$check == "Steady ramp with time (Spearman rho)" & fc$parameter == "turbidity", ]
  expect_equal(other$status, "ok")
})

test_that("duplicate and unsorted timestamps are handled and reported", {
  d <- read_csv(example_csv, show_col_types = FALSE)
  d <- rbind(d[1:100, ], d[100, ], d[101:500, ])[c(200:300, 1:199, 301:501), ]
  f <- tempfile(fileext = ".csv"); write_csv(d, f)
  x <- run_example(f)
  fc <- file_checks(x$read, x$grid, x$cfg)
  expect_equal(fc$status[fc$check == "Duplicate timestamps"], "check")
  expect_equal(fc$status[fc$check == "Rows out of time order in the file"], "check")
  expect_false(anyDuplicated(x$res$data$time) > 0)
})

test_that("a bad column name in the settings gives a clear error", {
  expect_error(run_example(overrides = "data.columns.turbidity=nonexistent"), "not in the file")
})

test_that("the summary tables are complete and add up for every combination of actions", {
  for (ov in list(c("defaults.spike.action_isolated=flag"), c("defaults.spike.action_sustained=remove"),
                  c("defaults.erratic.action=remove", "defaults.flat.action=remove", "defaults.range.action=flag"),
                  c("defaults.rate.action=flag", "defaults.rate.max_change=3", "defaults.erratic.action=off"))) {
    x <- run_example(overrides = ov); r <- x$res; r$cfg <- x$cfg
    s <- summary_table(r)
    expect_false(anyNA(s), info = paste(ov, collapse = " "))
    expect_equal(s$passed + s$removed + s$flagged_for_review, s$with_data, info = paste(ov, collapse = " "))
    o <- outcome_table(r)
    expect_false(anyNA(o$class), info = paste(ov, collapse = " "))
    expect_true(all(unique(unlist(r$data[grep("_flag$", names(r$data))])) %in% outcome_levels$outcome), info = paste(ov, collapse = " "))
    expect_equal(nrow(events_table(r)) > 0, TRUE)
  }
})
