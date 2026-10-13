test_that("overrides set nested values and the parameter-name shorthand works", {
  cfg <- load_config(overrides = c("turbidity.spike.min_dev=12", "defaults.spike.z_thresh=4", "strictness=2"))
  expect_equal(param_config(cfg, "turbidity")$spike$min_dev, 12)
  expect_equal(param_config(cfg, "ph")$spike$z_thresh, 4)
  expect_equal(cfg$strictness, 2)
  expect_equal(param_config(cfg, "ph")$spike$min_dev, 0.3)      # untouched
})

test_that("a parameter block overrides the defaults and keeps the rest", {
  cfg <- load_config()
  pc <- param_config(cfg, "spec_cond")
  expect_equal(pc$range$min, 20); expect_equal(pc$range$action, "remove")
  expect_equal(pc$spike$min_dev_rel, 0.05); expect_equal(pc$spike$window_hours, 3)
})

test_that("strictness must be positive and scaling moves thresholds the right way", {
  expect_error(load_config(strictness = 0))
  pc <- param_config(load_config(), "turbidity")
  hard <- scale_settings(pc, 2); easy <- scale_settings(pc, 0.5)
  expect_lt(hard$spike$z_thresh, pc$spike$z_thresh); expect_gt(easy$spike$z_thresh, pc$spike$z_thresh)
  expect_gt(hard$spike$corroborate$z_thresh, pc$spike$corroborate$z_thresh)   # harder for another sensor to confirm
  expect_equal(hard$range$max, pc$range$max)                                  # hard limits are never scaled
})

test_that("a settings file only needs the lines that change", {
  f <- tempfile(fileext = ".yml")
  writeLines(c("strictness: 1.5", "parameters:", "  turbidity:", "    range: {max: 1000}"), f)
  cfg <- load_config(f)
  expect_equal(cfg$strictness, 1.5)
  expect_equal(param_config(cfg, "turbidity")$range$max, 1000)
  expect_equal(param_config(cfg, "turbidity")$range$min, 0)
})

test_that("an unquoted off in YAML means off, and a bad action is rejected", {
  f <- tempfile(fileext = ".yml")
  writeLines(c("defaults:", "  flat:", "    action: off"), f)
  expect_equal(param_config(load_config(f), "ph")$flat$action, "off")
  expect_equal(param_config(load_config(overrides = "defaults.erratic.action=off"), "ph")$erratic$action, "off")
  expect_equal(param_config(load_config(), "ph")$rate$action, "off")
  expect_error(load_config(overrides = "defaults.flat.action=delete"), "must be remove, flag or off")
})
