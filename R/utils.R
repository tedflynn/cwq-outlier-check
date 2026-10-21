# Small helpers shared by the other files.

# Folder that holds config/default.yml, found by walking up from the working directory (or from
# SONDE_QC_ROOT if set). Lets the scripts and the Quarto report find the repo from wherever they run.
qc_root <- function() {
  env <- Sys.getenv("SONDE_QC_ROOT", unset = "")
  if (nzchar(env)) return(normalizePath(env))
  dir <- normalizePath(getwd())
  repeat {
    if (file.exists(file.path(dir, "config", "default.yml"))) return(dir)
    parent <- dirname(dir)
    if (parent == dir) stop("Could not find the cwq-outlier-check folder (config/default.yml). ",
                            "Run from inside the repo or set SONDE_QC_ROOT.", call. = FALSE)
    dir <- parent
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Number of readings in `hours` at `step_minutes` per reading (at least 1)
hours_to_n <- function(hours, step_minutes) max(1L, as.integer(round(hours * 60 / step_minutes)))

# TRUE for every position within +/- pad readings of a TRUE in `flag` (logical, no NAs, in time order).
# Cumulative sums keep it fast on series of a million readings.
near_flag <- function(flag, pad) {
  n <- length(flag)
  if (n == 0 || !any(flag)) return(rep(FALSE, n))
  cs <- cumsum(flag)
  idx <- seq_len(n)
  lo <- idx - pad - 1L
  (cs[pmin(idx + pad, n)] - ifelse(lo < 1L, 0L, cs[pmax(lo, 1L)])) > 0
}

# Run lengths for a logical vector: for each position, the length of the run of equal values it belongs to,
# and the run's id.
run_info <- function(x) {
  r <- rle(x)
  id <- rep(seq_along(r$lengths), r$lengths)
  list(id = id, len = r$lengths[id])
}
