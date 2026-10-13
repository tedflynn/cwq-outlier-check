# Running all the checks on a station's readings and resolving each reading to one outcome.

check_label <- c(range = "Range", rate = "Rate of change", erratic = "Erratic period", flat = "Flat line",
                 spike_isolated = "Isolated spike", spike_sustained = "Uncorroborated excursion")

# Every outcome a reading can end in, in the order they are shown: Valid, kept events, then for each check
# the flagged and removed versions (which one occurs depends on the check's action). A check's colour is the same
# for both; the version that is not the check's default action is drawn lighter.
check_default_action <- c(range = "remove", rate = "flag", erratic = "flag", flat = "flag", spike_isolated = "remove", spike_sustained = "flag")
check_colour <- c(range = "#7A0403", rate = "#117733", erratic = "#CC79A7", flat = "#0072B2", spike_isolated = "#D55E00", spike_sustained = "#E69F00")
lighten <- function(col, f = 0.5) grDevices::rgb(t(grDevices::col2rgb(col) * (1 - f) + 255 * f), maxColorValue = 255)

outcome_levels <- dplyr::bind_rows(
  tibble::tibble(outcome = c("Valid", "Kept: Corroborated event"), class = "passed", colour = c("#B8C4CE", "#4C9F70")),
  purrr::map_dfr(names(check_label), function(chk) {
    tibble::tibble(action = c("flag", "remove")) |>
      dplyr::mutate(outcome = ifelse(action == "flag", paste0("Flagged: ", check_label[[chk]], " (review)"), paste0("Removed: ", check_label[[chk]])),
                    class = ifelse(action == "flag", "review", "removed"),
                    colour = ifelse(action == check_default_action[[chk]], check_colour[[chk]], lighten(check_colour[[chk]])))
  }) |> dplyr::select(-action),
  tibble::tibble(outcome = "No data", class = "no data", colour = "#F2F2F2")
)

# "Removed: Range" or "Flagged: Range (review)" from a check and its action
outcome_for <- function(check, action) {
  lab <- check_label[[check]]
  switch(action, remove = paste0("Removed: ", lab), flag = paste0("Flagged: ", lab, " (review)"),
         stop("Unknown action '", action, "' for ", check, " (use remove, flag or off).", call. = FALSE))
}

# The sensitivity scaling by `strictness`: the thresholds that scale and how. See config/default.yml.
scale_settings <- function(pc, s) {
  pc$spike$z_thresh     <- pc$spike$z_thresh / s
  pc$spike$min_dev      <- pc$spike$min_dev / s
  pc$spike$min_dev_rel  <- pc$spike$min_dev_rel / s
  pc$spike$corroborate$z_thresh <- pc$spike$corroborate$z_thresh * s   # stricter: harder for another sensor to confirm
  pc$erratic$jump_thresh   <- pc$erratic$jump_thresh / s
  pc$erratic$jump_floor    <- pc$erratic$jump_floor / s
  pc$erratic$jump_multiple <- pc$erratic$jump_multiple / s
  pc$flat$min_hours <- pc$flat$min_hours / s
  pc$flat$auto_multiple <- pc$flat$auto_multiple / s
  if (!is.null(pc$rate$max_change)) pc$rate$max_change <- pc$rate$max_change / s
  pc
}

# Run every enabled check on every parameter that has data. `d` comes from regularize(). Returns:
#   data        one row per row of the file: time, and per parameter <p> (as read), <p>_flag (outcome),
#               <p>_clean (value with removed readings set to NA), <p>_checks (every check it failed)
#   checks      per parameter and check: how many readings failed it (checks can overlap) and its action
#   thresholds  the thresholds actually used per parameter (after strictness; includes the estimated erratic threshold)
run_checks <- function(d, cfg, progress = TRUE) {
  step <- cfg$data$step_minutes
  s <- cfg$strictness
  params <- parameter_names[parameter_names %in% names(d)]
  params <- params[vapply(params, function(p) any(!is.na(d[[p]])), logical(1))]
  if (length(params) == 0) stop("No parameter has any readings.", call. = FALSE)
  pc <- setNames(lapply(params, function(p) scale_settings(param_config(cfg, p), s)), params)
  say <- function(...) if (progress) message(...)

  # 1. Range. Readings that fail are left out of every later check, whatever the range action is.
  range_fail <- list(); x_ok <- list()
  for (p in params) {
    x <- d[[p]]
    rf <- if (pc[[p]]$range$action == "off") rep(FALSE, length(x)) else check_range(x, pc[[p]]$range$min, pc[[p]]$range$max)
    range_fail[[p]] <- rf
    x_ok[[p]] <- ifelse(rf, NA_real_, x)
  }

  # 2. Spikes, for every parameter first: each parameter's z-scores are what the others use to corroborate.
  spike <- list()
  for (p in params) {
    say("  ", pc[[p]]$label, ": rolling outlier check")
    sp <- pc[[p]]$spike
    spike[[p]] <- check_spike(x_ok[[p]], hours_to_n(sp$window_hours, step), sp$z_thresh, sp$min_dev, sp$min_dev_rel)
  }

  # 3. Everything else, then one outcome per reading.
  data <- tibble::tibble(time = d$time, in_file = d$in_file)
  checks <- list(); thresholds <- list()
  for (p in params) {
    cfgp <- pc[[p]]; x <- d[[p]]; n <- length(x)
    fails <- list()

    if (cfgp$range$action != "off") fails$range <- list(flag = range_fail[[p]], action = cfgp$range$action)

    if (!is.null(cfgp$rate$max_change) && cfgp$rate$action != "off")
      fails$rate <- list(flag = check_rate(x_ok[[p]], cfgp$rate$max_change), action = cfgp$rate$action)

    thr_erratic <- NA_real_
    if (cfgp$erratic$action != "off") {
      say("  ", cfgp$label, ": erratic periods")
      er <- check_erratic(x_ok[[p]], hours_to_n(cfgp$erratic$window_hours, step), cfgp$erratic$jump_method,
                          cfgp$erratic$jump_thresh, cfgp$erratic$jump_multiple, cfgp$erratic$jump_floor)
      fails$erratic <- list(flag = er$flag, action = cfgp$erratic$action)
      thr_erratic <- er$threshold
    }

    flat_n <- NA_integer_
    if (cfgp$flat$action != "off") {
      flat_n <- max(2L, flat_min_n(x_ok[[p]], hours_to_n(cfgp$flat$min_hours, step), cfgp$flat$min_hours_method,
                                   cfgp$flat$auto_multiple, cfgp$flat$auto_quantile, cfgp$flat$tolerance))
      fails$flat <- list(flag = check_flat(x_ok[[p]], flat_n, hours_to_n(cfgp$flat$pad_hours, step), cfgp$flat$tolerance),
                         action = cfgp$flat$action)
    }

    sp <- cfgp$spike
    runs <- classify_runs(spike[[p]]$outlier, hours_to_n(sp$isolated_max_hours, step))
    corr_with <- sp$corroborate$with
    others <- if (identical(corr_with, "all")) setdiff(params, p) else intersect(unlist(corr_with), setdiff(params, p))
    other_z <- do.call(cbind, c(lapply(others, function(o) spike[[o]]$z), list(matrix(NA_real_, n, 0))))
    corroborated <- corroborated_runs(runs$sustained, runs$run_id, other_z, sp$corroborate$z_thresh,
                                      if (sp$corroborate$pad_hours > 0) hours_to_n(sp$corroborate$pad_hours, step) else 0L)
    if (sp$action_isolated != "off") fails$spike_isolated <- list(flag = runs$isolated, action = sp$action_isolated)
    if (sp$action_sustained != "off") fails$spike_sustained <- list(flag = runs$sustained & !corroborated, action = sp$action_sustained)

    # One outcome per reading. Later assignments win, so the order below is the priority, lowest first
    # (the highest priority is range, then rate of change, erratic period, flat line, isolated spike, excursion).
    outcome <- rep("Valid", n)
    outcome[corroborated] <- "Kept: Corroborated event"
    for (chk in c("spike_sustained", "spike_isolated", "flat", "erratic", "rate", "range")) {
      f <- fails[[chk]]
      if (!is.null(f)) outcome[f$flag] <- outcome_for(chk, f$action)
    }
    outcome[is.na(x)] <- "No data"
    removed <- startsWith(outcome, "Removed")

    listed <- rep("", n)
    for (chk in names(fails)) {
      f <- fails[[chk]]$flag
      listed[f] <- ifelse(nzchar(listed[f]), paste0(listed[f], "; ", check_label[[chk]]), check_label[[chk]])
    }

    data[[p]] <- x
    data[[paste0(p, "_flag")]] <- outcome
    data[[paste0(p, "_clean")]] <- ifelse(removed, NA_real_, x)
    data[[paste0(p, "_checks")]] <- listed

    with_data <- sum(!is.na(x) & d$in_file)
    checks[[p]] <- tibble::tibble(parameter = p, check = names(fails),
                                  action = vapply(fails, `[[`, character(1), "action"),
                                  n_flagged = vapply(fails, function(f) sum(f$flag & d$in_file), integer(1)),
                                  n_with_data = with_data)
    thresholds[[p]] <- tibble::tibble(parameter = p, erratic_threshold = thr_erratic,
                                      spike_z = sp$z_thresh, spike_min_dev = sp$min_dev,
                                      spike_min_dev_rel = sp$min_dev_rel,
                                      flat_min_hours = flat_n * step / 60)
  }

  list(data = data[data$in_file, names(data) != "in_file"], params = params, cfg = cfg,
       checks = dplyr::bind_rows(checks), thresholds = dplyr::bind_rows(thresholds))
}
