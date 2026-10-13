test_that("range check flags values outside the limits and ignores missing readings", {
  x <- c(5, 10, NA, 4001, -1, 3999)
  expect_equal(check_range(x, 0, 4000), c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})

test_that("spike check finds a single large outlier and nothing in smooth data", {
  set.seed(1)
  x <- 10 + sin(seq(0, 20, length.out = 400)) + rnorm(400, 0, 0.1)
  expect_false(any(check_spike(x, 12, 3.5, 0, 0)$outlier))
  x[200] <- x[200] + 15
  sp <- check_spike(x, 12, 3.5, 0, 0)
  expect_equal(which(sp$outlier), 200)
})

test_that("the minimum deviation stops tiny differences counting when the local spread is near zero", {
  x <- rep(5, 100); x[50] <- 5.2
  expect_true(check_spike(x, 12, 3.5, 0, 0)$outlier[50])          # no floor: any difference from a flat series is an outlier
  expect_false(check_spike(x, 12, 3.5, 1, 0)$outlier[50])         # absolute floor
  y <- rep(500, 100); y[50] <- 520
  expect_false(check_spike(y, 12, 3.5, 0, 0.05)$outlier[50])      # relative floor: 20 is 4% of 500
  expect_true(check_spike(y, 12, 3.5, 0, 0.02)$outlier[50])
})

test_that("outlier runs split into isolated spikes and sustained excursions", {
  o <- rep(FALSE, 30); o[5] <- TRUE; o[10:11] <- TRUE; o[20:24] <- TRUE
  r <- classify_runs(o, isolated_n = 2)
  expect_equal(which(r$isolated), c(5, 10, 11))
  expect_equal(which(r$sustained), 20:24)
})

test_that("a sustained run is corroborated only when another sensor shows a signal during it", {
  sustained <- rep(FALSE, 40); sustained[10:14] <- TRUE; sustained[30:34] <- TRUE
  run_id <- run_info(sustained)$id
  z <- matrix(0, 40, 1); z[12, 1] <- 4
  out <- corroborated_runs(sustained, run_id, z, z_thresh = 2, pad = 0)
  expect_equal(which(out), 10:14)
  expect_false(any(corroborated_runs(sustained, run_id, matrix(0, 40, 0), 2, 0)))
  z2 <- matrix(0, 40, 1); z2[16, 1] <- 4                                     # just after the run
  expect_false(any(corroborated_runs(sustained, run_id, z2, 2, 0)[10:14]))
  expect_true(all(corroborated_runs(sustained, run_id, z2, 2, 3)[10:14]))     # unless padding reaches it
})

test_that("flat lines need the minimum run and are padded; missing readings break a run", {
  x <- c(rnorm(50), rep(3, 12), rnorm(50))
  f <- check_flat(x, min_n = 12, pad = 4, tolerance = 0)
  expect_equal(which(f), 47:66)
  expect_false(any(check_flat(c(rnorm(50), rep(3, 11), rnorm(50)), 12, 4, 0)))
  y <- c(rnorm(10), rep(3, 8), NA, rep(3, 8), rnorm(10))
  expect_false(any(check_flat(y, 12, 0, 0)))
  expect_true(any(check_flat(c(rnorm(10), rep(3, 12) + rnorm(12, 0, 0.001), rnorm(10)), 12, 0, tolerance = 0.01)))
})

test_that("erratic periods: fixed threshold, and auto scales with the station's own noise", {
  set.seed(2)
  calm <- rnorm(600, 10, 0.2)
  jumpy <- calm; jumpy[300:400] <- jumpy[300:400] + rnorm(101, 0, 20)
  fixed <- check_erratic(jumpy, 48, "fixed", 5, 10, 5)
  expect_true(sum(fixed$flag[300:400]) > 80)
  expect_equal(sum(fixed$flag[1:200]), 0)
  noisy <- rnorm(600, 30, 8)                            # normal variability is large here
  expect_gt(mean(check_erratic(noisy, 48, "fixed", 5, 10, 5)$flag), 0.9)   # a fixed 5 flags it all
  expect_equal(sum(check_erratic(noisy, 48, "auto", 5, 10, 5)$flag), 0)    # auto adapts
})

test_that("rate of change flags steps larger than the limit", {
  x <- c(1, 1.1, 1.2, 5, 5.1)
  expect_equal(check_rate(x, 1), c(FALSE, FALSE, FALSE, TRUE, FALSE))
})

test_that("flat-line threshold adapts to how often the station repeats values naturally", {
  set.seed(3)
  # a coarse sensor: values repeat for many readings by themselves, in runs of up to 40
  runs <- sample(c(1, 2, 4, 8, 20, 40), 400, replace = TRUE, prob = c(.3, .25, .2, .15, .07, .03))
  coarse <- rep(seq_along(runs) * 0.1, runs)
  expect_equal(flat_min_n(coarse, 12, "fixed"), 12)
  expect_gt(flat_min_n(coarse, 12, "auto", 2, 0.999), 40)
  smooth <- cumsum(rnorm(2000)) # almost no repeats
  expect_equal(flat_min_n(smooth, 12, "auto", 2, 0.999), 12)       # the floor wins
  stuck <- c(smooth[1:1000], rep(7, 30), smooth[1001:2000])
  n_use <- flat_min_n(stuck, 12, "auto", 2, 0.999)
  expect_true(any(check_flat(stuck, n_use, 0, 0)))                  # a stuck run is still found in a normal series
})
