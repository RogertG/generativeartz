#' Generate multiple generative images
#'
#' This is the main function of the package. It calls all the other functions necessary to
#' produce multiple generative images at once. Every image gets its own random seed, and the
#' seed, the formula and all parameters are written to the log file, so that every image can
#' be recreated with `regenerate_img()`.
#' @param formula the formula you want to use as a list with elements `x` and `y`
#' @param nr_of_img the number of images that should be created
#' @param polar logical should the plot have a polar coordinate system ("polar = TRUE") or a
#'   cartesian coordinate system ("polar = FALSE")
#' @param filetype file type of the final image. Default is `png`, for other options see the
#'   `device` argument in `ggplot2::ggsave()`
#' @param ... further parameters: `color`, `background_color`, `alpha`, `size`, `shape`,
#'   `width`, `height` (see `generate_plot()`) and `range_from`, `range_to`, `step`
#'   (see `generate_data()`)
#' @param img_path directory the images are saved in. Defaults to the global variable
#'   `IMG_PATH` if it exists, otherwise `"img/everything/"`.
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return the paths of the saved images (invisibly)
#' @seealso \code{\link{generate_seeds}} generate the seeds for the randomness
#' @seealso \code{\link{generate_filename}} generate the file names
#' @seealso \code{\link{check_logfile_existence}} create a log file, if there is none
#' @seealso \code{\link{generate_logfile_entry}} generate the specific entry for the log file
#' @seealso \code{\link{generate_data}} generate the data depending on the formula
#' @seealso \code{\link{generate_plot}} plot the data and save an image file
#' @export
#' @examples
#' \dontrun{
#' my_formula <- list(
#'   x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
#'   y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
#' )
#' generate_img(formula = my_formula, nr_of_img = 3, polar = FALSE)
#' generate_img(formula = my_formula, nr_of_img = 3, polar = TRUE,
#'              color = "#101820", background_color = "#F2AA4C", alpha = 0.1)
#' }
#' @importFrom purrr map_chr

generate_img <- function(formula, nr_of_img, polar = FALSE, filetype = "png", ...,
                         img_path = default_img_path(),
                         logfile_path = default_logfile_path()) {
  validate_formula(formula)
  params <- resolve_params(c(list(polar = polar), list(...)))
  seeds <- generate_seeds(nr_of_img)
  paths <- purrr::map_chr(seeds, function(seed) {
    render_img(formula, seed, params, filetype, img_path, logfile_path)
  })
  invisible(paths)
}

# Create, log and save a single image for a given seed.
render_img <- function(formula, seed, params, filetype, img_path, logfile_path) {
  set.seed(seed)
  file_name <- generate_filename(seed, filetype)
  logfile <- check_logfile_existence(logfile_path)
  generate_logfile_entry(logfile, formula, seed, file_name, params, logfile_path)
  df <- generate_data(formula, range_from = params$range_from,
                      range_to = params$range_to, step = params$step)
  generate_plot(df, file_name, polar = params$polar, filetype = filetype,
                color = params$color, background_color = params$background_color,
                alpha = params$alpha, size = params$size, shape = params$shape,
                width = params$width, height = params$height, img_path = img_path)
}
