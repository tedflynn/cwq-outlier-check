# The individual checks. Each takes the readings of ONE parameter as a numeric vector in time order (on the
# regular grid from regularize(), NA where there is no reading) plus the settings for that check, and returns
# logicals or numbers of the same length. They use only that sonde's own readings.
#
# Windows come in as numbers of readings (the callers convert hours with hours_to_n()). Thresholds come in
# already scaled by `strictness` (see scale_settings() in R/pipeline.R).

roll_median <- function(x, w) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  slider::slide_dbl(x, function(v) median(v, na.rm = TRUE), .before = w, .after = w)
}
roll_mad <- function(x, w) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  slider::slide_dbl(x, function(v) if (all(is.na(v))) NA_real_ else mad(v, na.rm = TRUE), .before = w, .after = w)
}

# Range: readings outside [min, max]
check_range <- function(x, min, max) {
  !is.na(x) & (x < min | x > max)
}

# Robust local outliers. A reading is an outlier when it is far from the rolling median in two ways at once:
#   modified z = 0.6745 * (x - rolling median) / rolling MAD   exceeds z_thresh (Iglewicz and Hoaglin 1993), and
#   |x - rolling median| exceeds max(min_dev, min_dev_rel * |rolling median|)
# The second condition stops trivial differences from counting when the local spread is almost zero.
# Returns the outlier flag and the z-score (the z-scores are also what other parameters use to corroborate).
check_spike <- function(x, window, z_thresh, min_dev, min_dev_rel) {
  med <- roll_median(x, window)
  spread <- roll_mad(x, window)
  z <- 0.6745 * (x - med) / spread
  floor <- pmax(min_dev, min_dev_rel * abs(med))
  out <- !is.na(x) & (abs(x - med) > floor) %in% TRUE & (abs(z) > z_thresh) %in% TRUE
  list(outlier = out, z = z)
}

# Split outlier runs into isolated spikes (at most `isolated_n` consecutive readings) and sustained
# excursions (longer). A run is a stretch of consecutive grid readings that are all outliers.
classify_runs <- function(outlier, isolated_n) {
  ri <- run_info(outlier)
  list(isolated = outlier & ri$len <= isolated_n, sustained = outlier & ri$len > isolated_n, run_id = ri$id)
}

# Does another sensor confirm a sustained excursion? `other_z` is a matrix of modified z-scores of the other
# sensors (columns). A run is corroborated if any other sensor's |z| exceeds `z_thresh` at some reading of the run
# (or within `pad` readings of it). Returns TRUE for every reading of a corroborated sustained run.
corroborated_runs <- function(sustained, run_id, other_z, z_thresh, pad) {
  if (!any(sustained) || ncol(other_z) == 0) return(rep(FALSE, length(sustained)))
  signal <- rowSums(abs(other_z) > z_thresh, na.rm = TRUE) > 0
  if (pad > 0) signal <- near_flag(signal, pad)
  run_signal <- tapply(signal[sustained], run_id[sustained], any)
  sustained & as.logical(run_signal[as.character(run_id)]) %in% TRUE
}

# Erratic periods: a stretch where the median size of the change between consecutive readings, taken over a
# window, is above a threshold. Catches disturbances longer than the spike window. Returns the flag and the
# threshold that was used (it is estimated from the data in the "auto" method).
check_erratic <- function(x, window, method, jump_thresh, jump_multiple, jump_floor) {
  jump <- abs(x - c(NA, x[-length(x)]))
  thr <- if (identical(method, "auto")) max(jump_floor, jump_multiple * median(jump, na.rm = TRUE)) else jump_thresh
  med_jump <- roll_median(jump, window)
  list(flag = !is.na(x) & !is.na(med_jump) & med_jump > thr, threshold = thr, median_jump = med_jump)
}

# Runs of identical readings (within `tolerance`): the run id of every reading and the length of its run. A missing
# reading breaks a run.
flat_runs <- function(x, tolerance = 0) {
  n <- length(x)
  same <- c(FALSE, abs(x[-1] - x[-n]) <= tolerance)
  same[is.na(same)] <- FALSE
  run_id <- cumsum(!same)
  list(id = run_id, len = tabulate(run_id)[run_id])
}

# How many identical readings in a row make a flat line. How often a sensor repeats a value naturally depends on its
# resolution and on how steady the water is (temperature reported to 0.1 C can sit on one value for most of a day),
# so with method "auto" the threshold is the larger of `min_n` and `multiple` times the station's own `quantile` of
# natural run length. "fixed" uses `min_n` alone.
flat_min_n <- function(x, min_n, method = "fixed", multiple = 2, quantile = 0.999, tolerance = 0) {
  if (!identical(method, "auto") || sum(!is.na(x)) < 2) return(min_n)
  r <- flat_runs(x, tolerance)
  lens <- tabulate(r$id[!is.na(x)])
  lens <- lens[lens > 0]
  max(min_n, ceiling(multiple * as.numeric(stats::quantile(lens, quantile, names = FALSE))))
}

# Flat lines: `min_n` or more identical readings (within `tolerance`) in a row, plus `pad` readings either side.
check_flat <- function(x, min_n, pad, tolerance) {
  n <- length(x)
  if (n < 2) return(rep(FALSE, n))
  r <- flat_runs(x, tolerance)
  near_flag(!is.na(x) & r$len >= min_n, pad)
}

# Rate of change: the step from the previous reading is larger than the sensor can physically show
check_rate <- function(x, max_change) {
  jump <- abs(x - c(NA, x[-length(x)]))
  !is.na(jump) & jump > max_change
}
