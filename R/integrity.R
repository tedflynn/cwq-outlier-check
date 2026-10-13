# File-level checks. These look for problems with the file itself (damaged or sorted columns, duplicate or
# missing times, parameters that are missing or constant). They report; they never change the data.
# `status` is "ok" or "check".

file_checks <- function(read, grid, cfg) {
  step <- cfg$data$step_minutes
  rg <- attr(grid, "regularize")
  d <- grid[grid$in_file, ]
  rows <- list()
  add <- function(check, parameter = "", value, status, note = "") {
    rows[[length(rows) + 1]] <<- tibble::tibble(check = check, parameter = parameter, value = as.character(value),
                                                status = status, note = note)
  }

  add("Rows read", value = format(read$rows_read, big.mark = ","), status = "ok",
      note = paste0("Period ", format(min(d$time), "%Y-%m-%d"), " to ", format(max(d$time), "%Y-%m-%d"),
                    "; time column '", paste(read$timestamp_column, collapse = " + "), "', format '", read$timestamp_format, "'"))
  add("Unreadable date-times", value = read$unparsed_times, status = if (read$unparsed_times > 0) "check" else "ok",
      note = "Rows whose date-time could not be parsed are dropped")
  add("Rows out of time order in the file", value = read$unsorted, status = if (read$unsorted > 0) "check" else "ok",
      note = "The tool sorts by time before checking")
  add("Duplicate timestamps", value = rg$duplicates, status = if (rg$duplicates > 0) "check" else "ok",
      note = "Only the first reading at a repeated time is kept")
  add(paste0("Timestamps not on the ", step, "-minute grid"), value = rg$off_grid, status = if (rg$off_grid > 0) "check" else "ok",
      note = "Rounded to the nearest step")
  expected <- nrow(grid)
  add("Missing time steps (no row at all)", value = format(rg$filled, big.mark = ","),
      status = if (rg$filled / expected > 0.02) "check" else "ok",
      note = paste0(round(100 * rg$filled / expected, 2), "% of the ", format(expected, big.mark = ","), " steps in the period"))

  # Gaps: stretches with no reading at all (in any parameter)
  has_any <- Reduce(`|`, lapply(intersect(parameter_names, names(d)), function(p) !is.na(grid[[p]])))
  gap <- run_info(!has_any)
  gap_len <- unique(data.frame(id = gap$id[!has_any], len = gap$len[!has_any]))
  long <- sum(gap_len$len * step / 60 > cfg$integrity$max_gap_hours)
  add(paste0("Gaps longer than ", cfg$integrity$max_gap_hours, " hours"), value = long, status = if (long > 0) "check" else "ok",
      note = if (nrow(gap_len)) paste0("Longest gap ", round(max(gap_len$len) * step / 60, 1), " hours") else "No gaps")

  found <- parameter_names[parameter_names %in% names(d)]
  missing <- setdiff(parameter_names, found)
  add("Parameters not found in the file", value = if (length(missing)) paste(missing, collapse = ", ") else "none",
      status = "ok", note = "These are skipped")

  tt <- as.numeric(d$time)
  for (p in found) {
    x <- d[[p]]; ok <- !is.na(x)
    if (sum(ok) == 0) { add("Has readings", p, "none", "check"); next }
    add("Share of rows with a reading", p, paste0(round(100 * mean(ok), 1), "%"), if (mean(ok) < 0.8) "check" else "ok")
    if (length(unique(x[ok])) == 1) add("Constant column", p, unique(x[ok]), "check", "Every reading is the same value")
    if (sum(ok) >= 1000) {
      rho <- suppressWarnings(cor(x[ok], tt[ok], method = "spearman"))
      add("Steady ramp with time (Spearman rho)", p, sprintf("%.2f", rho), if (abs(rho) >= cfg$integrity$ramp_rho) "check" else "ok",
          "Near 1 or -1 means a sorted or damaged column; real long-term change is usually well below 0.8")
    }
  }
  dplyr::bind_rows(rows)
}
