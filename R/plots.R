# Diagnostic plots, used by the Quarto report. They show every flagged reading against the cleaned series so
# thresholds can be judged by eye.

outcome_colours <- function() setNames(outcome_levels$colour, outcome_levels$outcome)
log_parameters <- c("turbidity", "spec_cond", "chlorophyll")

# Whole record: daily median of the cleaned series, with a marker for each day that had flagged readings, placed at
# that day's most extreme flagged reading. Markers outside the plotted range (error codes such as 9999) are not drawn.
plot_overview <- function(res, p) {
  d <- res$data
  day <- lubridate::as_date(d$time)
  flag <- d[[paste0(p, "_flag")]]; x <- d[[p]]; clean <- d[[paste0(p, "_clean")]]
  daily <- tibble::tibble(day = day, clean = clean) |>
    dplyr::group_by(day) |>
    dplyr::summarise(median = if (sum(!is.na(clean)) >= 12) median(clean, na.rm = TRUE) else NA_real_, .groups = "drop")
  bad <- !flag %in% c("Valid", "No data", "Kept: Corroborated event")
  pts <- tibble::tibble(day = day[bad], outcome = flag[bad], value = x[bad]) |>
    dplyr::group_by(day, outcome) |>
    dplyr::slice_max(abs(value - median(daily$median, na.rm = TRUE)), n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  use_log <- p %in% log_parameters
  rng <- range(daily$median, na.rm = TRUE); pad <- diff(rng) * 0.25
  lim <- if (use_log) c(max(min(rng) / 3, 1e-3), max(rng) * 8) else c(rng[1] - pad, rng[2] + pad)
  pts <- dplyr::filter(pts, value >= lim[1], value <= lim[2], !use_log | value > 0)
  g <- ggplot2::ggplot() +
    ggplot2::geom_line(data = daily, ggplot2::aes(day, median), colour = "grey35", linewidth = 0.35) +
    ggplot2::geom_point(data = pts, ggplot2::aes(day, value, colour = outcome), size = 1.2, alpha = 0.75) +
    ggplot2::scale_colour_manual(values = outcome_colours(), name = NULL, limits = intersect(outcome_levels$outcome, pts$outcome)) +
    ggplot2::labs(x = NULL, y = param_config(res$cfg, p)$label, title = paste(param_config(res$cfg, p)$label, "- daily median and flagged readings")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
  if (use_log) g + ggplot2::scale_y_log10(limits = lim) else g + ggplot2::coord_cartesian(ylim = lim)
}

# The window of `days` days with the most removed or flagged readings (a week by default), drawn reading by reading
plot_zoom <- function(res, p, days = 7) {
  d <- res$data
  flag <- d[[paste0(p, "_flag")]]; x <- d[[p]]
  bad <- !flag %in% c("Valid", "No data", "Kept: Corroborated event")
  if (!any(bad)) return(NULL)
  day <- lubridate::as_date(d$time)
  per_day <- tapply(bad, day, sum)
  d0 <- as.Date(names(per_day))
  score <- vapply(d0, function(a) sum(per_day[d0 >= a & d0 < a + days]), numeric(1))
  best <- which(score == max(score))
  start <- d0[best[ceiling(length(best) / 2)]]          # of the equally good windows, the middle one centres the events
  w <- day >= start & day < start + days
  seg <- tibble::tibble(time = d$time[w], value = x[w], outcome = flag[w])
  keep <- seg$value[seg$outcome == "Valid"]
  lim <- if (length(keep) > 10) range(keep, na.rm = TRUE) + c(-1, 1) * diff(range(keep, na.rm = TRUE)) * 0.6 else range(seg$value, na.rm = TRUE)
  ggplot2::ggplot(seg, ggplot2::aes(time, value)) +
    ggplot2::geom_line(colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_point(data = dplyr::filter(seg, !outcome %in% c("Valid", "No data")), ggplot2::aes(colour = outcome), size = 1.6) +
    ggplot2::scale_colour_manual(values = outcome_colours(), name = NULL, limits = intersect(outcome_levels$outcome, seg$outcome[!seg$outcome %in% c("Valid", "No data")])) +
    ggplot2::coord_cartesian(ylim = lim) +
    ggplot2::labs(x = NULL, y = param_config(res$cfg, p)$label,
                  title = paste0(param_config(res$cfg, p)$label, ": the ", days, " days from ", format(start), " with the most flagged readings")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}
