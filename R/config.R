# Reading, merging and overriding the settings (config/default.yml).

parameter_names <- c("turbidity", "water_temp", "ph", "spec_cond", "dissolved_oxygen", "chlorophyll")

# Settings for the run: default.yml, then the user's settings file (only the lines it contains), then single
# overrides given as "path=value" strings (for example "turbidity.spike.min_dev=10" or "strictness=1.5"),
# then `strictness` if given directly. A path that starts with a parameter name refers to that parameter.
load_config <- function(path = NULL, strictness = NULL, overrides = character()) {
  cfg <- yaml::read_yaml(file.path(qc_root(), "config", "default.yml"))
  if (!is.null(path)) cfg <- modifyList(cfg, yaml::read_yaml(path), keep.null = TRUE)
  for (o in overrides) cfg <- apply_override(cfg, o)
  if (!is.null(strictness)) cfg$strictness <- strictness
  cfg <- normalize_actions(cfg)
  if (!is.numeric(cfg$strictness) || length(cfg$strictness) != 1 || cfg$strictness <= 0)
    stop("strictness must be a single number above 0.", call. = FALSE)
  cfg
}

apply_override <- function(cfg, text) {
  parts <- strsplit(text, "=", fixed = TRUE)[[1]]
  if (length(parts) < 2) stop("An override must look like path=value, got: ", text, call. = FALSE)
  keys <- strsplit(parts[1], ".", fixed = TRUE)[[1]]
  value <- yaml::yaml.load(paste(parts[-1], collapse = "="))
  if (keys[1] %in% parameter_names) keys <- c("parameters", keys)
  set_path(cfg, keys, value)
}

set_path <- function(x, keys, value) {
  if (length(keys) == 1) { x[keys] <- list(value); return(x) }
  x[[keys[1]]] <- set_path(x[[keys[1]]] %||% list(), keys[-1], value)
  x
}

# The settings that apply to one parameter: the defaults overridden by that parameter's own block
param_config <- function(cfg, p) {
  out <- modifyList(cfg$defaults, cfg$parameters[[p]] %||% list(), keep.null = TRUE)
  out$label <- out$label %||% p
  out$name <- p
  out
}

# One row per setting actually used, for the report and for the saved copy of the settings
settings_table <- function(cfg, params) {
  flat <- function(x, prefix = "") {
    out <- list()
    for (n in names(x)) {
      key <- if (nzchar(prefix)) paste(prefix, n, sep = ".") else n
      if (is.list(x[[n]]) && !is.null(names(x[[n]]))) out <- c(out, flat(x[[n]], key))
      else out[[key]] <- paste(unlist(x[[n]]), collapse = ", ")
    }
    out
  }
  purrr::map_dfr(params, function(p) {
    f <- flat(param_config(cfg, p)[setdiff(names(param_config(cfg, p)), c("label", "name"))])
    tibble::tibble(parameter = p, setting = names(f), value = unlist(f, use.names = FALSE))
  })
}

# YAML reads an unquoted `off` as FALSE (and `on` as TRUE), so `action: off` arrives as a logical. Turn it back
# into the word, and reject anything that is not remove, flag or off, in every `action*` setting.
normalize_actions <- function(x, path = "") {
  if (!is.list(x)) return(x)
  for (n in names(x)) {
    here <- if (nzchar(path)) paste(path, n, sep = ".") else n
    if (startsWith(n, "action")) {
      v <- x[[n]]
      if (is.logical(v) && length(v) == 1 && !is.na(v) && !v) v <- "off"
      if (!(is.character(v) && length(v) == 1 && v %in% c("remove", "flag", "off")))
        stop("Setting '", here, "' must be remove, flag or off.", call. = FALSE)
      x[[n]] <- v
    } else if (is.list(x[[n]])) x[[n]] <- normalize_actions(x[[n]], here)
  }
  x
}
