# Internal helpers --------------------------------------------------------

# Paths default to the global variables used in the README (IMG_PATH,
# LOGFILE_PATH) so existing scripts keep working, but can be passed explicitly.
default_img_path <- function() {
  get0("IMG_PATH", envir = globalenv(), ifnotfound = "img/everything/")
}

default_logfile_path <- function() {
  get0("LOGFILE_PATH", envir = globalenv(), ifnotfound = "logfile/logfile.csv")
}

# Default rendering/data parameters. Every value is recorded in the log file,
# so an image can be recreated exactly even if these defaults change later.
default_params <- function() {
  list(
    polar = FALSE,
    color = "black",
    background_color = "white",
    alpha = 0.2,
    size = 0,
    shape = 46,
    range_from = -2 * pi,
    range_to = 2 * pi,
    step = 0.01,
    width = 15,
    height = 15
  )
}

# Fill unspecified parameters with defaults and reject unknown ones.
resolve_params <- function(params) {
  defaults <- default_params()
  unknown <- setdiff(names(params), names(defaults))
  if (length(unknown) > 0) {
    stop("Unknown argument(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  params <- params[!vapply(params, is.null, logical(1))]
  utils::modifyList(defaults, params)
}

validate_formula <- function(formula) {
  if (!is.list(formula) || !all(c("x", "y") %in% names(formula))) {
    stop("`formula` must be a list with elements `x` and `y`, e.g. ",
         "list(x = quote(sin(x_i)), y = quote(cos(y_i))).", call. = FALSE)
  }
  invisible(formula)
}

empty_logfile <- function() {
  defaults <- default_params()
  log_params <- defaults[setdiff(names(defaults), c("width", "height"))]
  cbind(
    data.frame(file_name = character(0), seed = integer(0),
               formula_x = character(0), formula_y = character(0),
               stringsAsFactors = FALSE),
    as.data.frame(lapply(log_params, function(v) v[0]), stringsAsFactors = FALSE)
  )
}
