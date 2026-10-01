#' Recreate an image
#'
#' This is the main function to recreate a previously generated image. It takes all relevant
#' information from the log file to get exactly the same result: by using the same formula and
#' the same seed you get the same random numbers. The plot parameters (polar, colors, alpha,
#' etc.) are taken from the log file as well, but can be overridden, e.g. to render the same
#' image in another color. Log entries written by older versions of the package do not contain
#' these parameters; the current defaults are used for them.
#' @param seed_to_recreate the seed of the original image
#' @param filetype set the file type for the image
#' @param polar logical, overrides the coordinate system stored in the log file
#' @param ... further parameters overriding the logged ones, see `generate_img()`
#' @param img_path directory the image is saved in. Defaults to the global variable
#'   `IMG_PATH` if it exists, otherwise `"img/everything/"`.
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return the path of the saved image (invisibly)
#' @seealso \code{\link{get_formula_from_logfile}} get the formula for recreation
#' @seealso \code{\link{get_seed_from_logfile}} get the seed, which is used as input
#' @export
#' @examples
#' \dontrun{
#' regenerate_img(104)
#' regenerate_img(104, color = "white", background_color = "black")
#' }

regenerate_img <- function(seed_to_recreate, filetype = "png", polar = NULL, ...,
                           img_path = default_img_path(),
                           logfile_path = default_logfile_path()) {
  entry <- get_logfile_entry(seed_to_recreate, logfile_path)
  formula <- get_formula_from_logfile(seed_to_recreate, logfile_path = logfile_path)
  logged <- entry[intersect(names(entry), names(default_params()))]
  logged <- Filter(function(v) !is.na(v), as.list(logged))
  overrides <- c(list(polar = polar), list(...))
  params <- resolve_params(utils::modifyList(logged, overrides[!vapply(overrides, is.null, logical(1))]))
  path <- render_img(formula, entry$seed, params, filetype, img_path, logfile_path)
  invisible(path)
}
