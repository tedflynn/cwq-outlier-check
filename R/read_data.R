# Reading a station CSV: find the date-time and parameter columns, parse them, and place the readings on a
# regular time grid so the rolling windows (given in hours) mean the same thing everywhere.

# Names a parameter's column is commonly given. Compared after lower-casing and dropping everything but
# letters and digits, so "Water_Temperature", "water temperature" and "watertemperature" all match.
column_aliases <- list(
  turbidity        = c("turbidity", "turb", "turbfnu", "turbntu", "turbidityfnu", "turbidityntu"),
  water_temp       = c("watertemp", "watertemperature", "temperature", "temp", "watertempc", "watertemperaturec"),
  ph               = c("ph"),
  spec_cond        = c("speccond", "spcond", "spc", "specificconductance", "specificconductivity",
                       "spconduscm", "conductivity", "sc", "ec"),
  dissolved_oxygen = c("dissolvedoxygen", "do", "domgl", "dissolvedoxygenmgl", "odo", "doconc"),
  chlorophyll      = c("chlorophyll", "chlorophylla", "chlor", "chlorugl", "chl", "chla", "fluorescence")
)
norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))

timestamp_aliases <- c("datetime", "datetimepst", "timestamp", "dateandtime", "datetimeutc", "datetimelocal")

# Which column holds each parameter, as a named character vector (NA where none was found).
find_columns <- function(header, cfg) {
  spec <- cfg$data$columns
  out <- setNames(rep(NA_character_, length(parameter_names)), parameter_names)
  for (p in parameter_names) {
    want <- spec[[p]]
    if (is.null(want)) next
    if (!identical(want, "auto")) {
      if (!want %in% header) stop("Column '", want, "' (for ", p, ") is not in the file. Columns found: ",
                                  paste(header, collapse = ", "), call. = FALSE)
      out[p] <- want
    } else {
      hit <- header[norm_name(header) %in% column_aliases[[p]]]
      if (length(hit) >= 1) out[p] <- hit[1]
    }
  }
  if (anyDuplicated(out[!is.na(out)])) stop("Two parameters were matched to the same column; name them in `data.columns`.", call. = FALSE)
  out
}

parse_orders <- c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM", "ymd", "mdy")

# Date-time column(s) in `d` (all character) parsed to POSIXct in the clock of the file.
parse_timestamps <- function(d, cfg) {
  tz <- cfg$data$timezone
  want <- cfg$data$timestamp
  header <- names(d)
  if (identical(want, "auto")) {
    hit <- header[norm_name(header) %in% timestamp_aliases]
    if (length(hit) >= 1) {
      text <- d[[hit[1]]]; used <- hit[1]
    } else {
      dcol <- header[norm_name(header) == "date"]; tcol <- header[norm_name(header) == "time"]
      if (length(dcol) == 1 && length(tcol) == 1) {
        text <- paste(d[[dcol]], d[[tcol]]); used <- c(dcol, tcol)
      } else stop("Could not find the date-time column. Name it in `data.timestamp`. Columns found: ",
                  paste(header, collapse = ", "), call. = FALSE)
    }
  } else {
    if (!want %in% header) stop("Timestamp column '", want, "' is not in the file.", call. = FALSE)
    text <- d[[want]]; used <- want
  }
  orders <- if (identical(cfg$data$timestamp_format, "auto")) parse_orders else cfg$data$timestamp_format
  sample_text <- head(text[!is.na(text) & nzchar(text)], 2000)
  fits <- vapply(orders, function(o) mean(!is.na(suppressWarnings(lubridate::parse_date_time(sample_text, o, tz = tz, quiet = TRUE)))), numeric(1))
  best <- orders[which.max(fits)]
  if (max(fits) < 0.9) stop("Could not read the date-times (best format '", best, "' parsed ", round(100 * max(fits)),
                           "% of a sample). Set `data.timestamp_format`.", call. = FALSE)
  list(time = suppressWarnings(lubridate::parse_date_time(text, best, tz = tz, quiet = TRUE)), column = used, format = best)
}

# Read a station CSV. Returns the readings (one row per row of the file, time-ordered) with the standard
# parameter columns, plus what was found.
read_station_csv <- function(path, cfg) {
  d <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       na = cfg$data$missing_values, show_col_types = FALSE, progress = FALSE)
  names(d) <- trimws(sub("^﻿", "", names(d)))
  ts <- parse_timestamps(d, cfg)
  cols <- find_columns(names(d), cfg)
  if (all(is.na(cols))) stop("None of the six parameters were found in the file. Columns: ", paste(names(d), collapse = ", "),
                             ". Name them in `data.columns`.", call. = FALSE)
  out <- tibble::tibble(time = ts$time)
  for (p in parameter_names) {
    if (!is.na(cols[p])) out[[p]] <- suppressWarnings(as.numeric(d[[cols[p]]]))
  }
  bad_time <- is.na(out$time)
  out <- out[!bad_time, ]
  unsorted <- sum(diff(as.numeric(out$time)) < 0)       # rows that come earlier in time than the row before them
  out <- out[order(out$time), ]
  list(data = out, columns = cols, timestamp_column = ts$column, timestamp_format = ts$format,
       rows_read = nrow(d), unparsed_times = sum(bad_time), unsorted = unsorted)
}

# Put the readings on the regular grid: timestamps are rounded to the step, duplicates beyond the first are
# dropped (and counted), and missing times are filled with empty readings (`in_file` FALSE). The checks run
# on the full grid; results are reported only for rows that were in the file.
regularize <- function(d, step_minutes) {
  step <- step_minutes * 60
  t <- lubridate::round_date(d$time, paste(step_minutes, "mins"))
  off_grid <- sum(abs(as.numeric(d$time) - as.numeric(t)) > 1)
  d$time <- t
  dup <- duplicated(d$time)
  d <- d[!dup, ]
  grid <- tibble::tibble(time = seq(min(d$time), max(d$time), by = step))
  out <- dplyr::left_join(grid, dplyr::mutate(d, in_file = TRUE), by = "time")
  out$in_file <- !is.na(out$in_file)
  attr(out, "regularize") <- list(off_grid = off_grid, duplicates = sum(dup), filled = sum(!out$in_file))
  out
}
