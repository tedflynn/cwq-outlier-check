# Summary tables of what the checks found and removed. All of them count readings in the file (not the
# empty rows added to fill gaps in the time grid).

# passed, removed, review or no data, from the outcome's wording ("Removed: ...", "Flagged: ... (review)", ...)
outcome_class <- function(outcome) {
  ifelse(outcome == "No data", "no data", ifelse(startsWith(outcome, "Removed"), "removed",
         ifelse(startsWith(outcome, "Flagged"), "review", "passed")))
}

# Every outcome per parameter: how many readings, and the share of the parameter's readings with data.
# Outcomes that did not occur are left out, except "Valid".
outcome_table <- function(res) {
  purrr::map_dfr(res$params, function(p) {
    flag <- res$data[[paste0(p, "_flag")]]
    with_data <- sum(flag != "No data")
    tab <- table(factor(flag, levels = outcome_levels$outcome))
    tibble::tibble(parameter = p, outcome = names(tab), n = as.integer(tab)) |>
      dplyr::filter(outcome != "No data", n > 0 | outcome == "Valid") |>
      dplyr::mutate(class = outcome_class(outcome), pct = 100 * n / with_data)
  })
}

# One row per parameter: the headline numbers
summary_table <- function(res) {
  purrr::map_dfr(res$params, function(p) {
    flag <- res$data[[paste0(p, "_flag")]]
    cls <- outcome_class(flag)
    with_data <- sum(cls != "no data")
    tibble::tibble(parameter = p, label = param_config(res$cfg, p)$label, rows = length(flag),
                   with_data = with_data, passed = sum(cls == "passed"), removed = sum(cls == "removed"),
                   flagged_for_review = sum(cls == "review"),
                   pct_passed = 100 * sum(cls == "passed") / with_data, pct_removed = 100 * sum(cls == "removed") / with_data,
                   pct_flagged = 100 * sum(cls == "review") / with_data)
  })
}

# Per check: how many readings failed it. A reading can fail several checks, so these do not add up to the
# totals above (the outcome table does).
check_table <- function(res) {
  res$checks |>
    dplyr::mutate(check = unname(check_label[check]), pct = 100 * n_flagged / n_with_data)
}

# Runs of consecutive readings with the same flagged outcome ("events"), with how far they stood above (or below)
# the valid readings in the 24 hours before.
events_table <- function(res, step_minutes = res$cfg$data$step_minutes) {
  purrr::map_dfr(res$params, function(p) {
    flag <- res$data[[paste0(p, "_flag")]]; x <- res$data[[p]]; t <- res$data$time
    r <- rle(flag)
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    keep <- !r$values %in% c("Valid", "No data")
    if (!any(keep)) return(NULL)
    ok <- flag == "Valid"
    back <- 24 * 60 / step_minutes
    purrr::pmap_dfr(list(starts[keep], ends[keep], r$values[keep]), function(a, b, o) {
      idx <- max(1, a - back):max(1, a - 1)
      seg <- x[a:b]
      tibble::tibble(parameter = p, outcome = o, start = t[a], end = t[b], readings = b - a + 1,
                     hours = (b - a + 1) * step_minutes / 60, min_value = min(seg, na.rm = TRUE), max_value = max(seg, na.rm = TRUE),
                     baseline = median(x[idx][ok[idx]], na.rm = TRUE))
    })
  })
}
