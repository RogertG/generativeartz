#' Generate one generative plot
#'
#' This function plots previously created data and saves it to a file.
#' @param df the data frame created with `generate_data()`
#' @param file_name the file name for saving the plot
#' @param polar do you want to use a polar coordinate system? The default is a cartesian coordinate system.
#' @param filetype file type of the final image. Default is `png`, for other options see the
#'   `device` argument in `ggplot2::ggsave()`
#' @param color color of the points. Default is black.
#' @param background_color background color of the plot. Default is white.
#' @param alpha transparency of the points, between 0 and 1.
#' @param size size of the points.
#' @param shape shape of the points. The default `46` draws single pixels (`"."`),
#'   which is fast and gives fine textures; `20` gives small dots.
#' @param width,height size of the saved image in inches.
#' @param img_path directory the image is saved in. Defaults to the global variable
#'   `IMG_PATH` if it exists, otherwise `"img/everything/"`.
#' @return the path of the saved image (invisibly)
#' @seealso \code{\link{generate_data}} where the data is created
#' @export
#' @examples
#' \dontrun{
#' generate_plot(df, file_name, polar = FALSE)
#' }
#' @import ggplot2
#' @importFrom magrittr %>%

generate_plot <- function(df, file_name, polar = FALSE, filetype = "png", color = "black",
                          background_color = "white", alpha = 0.2, size = 0, shape = 46,
                          width = 15, height = 15, img_path = default_img_path()) {
  message("generate plot")
  plot <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(alpha = alpha, size = size, shape = shape, color = color) +
    ggplot2::theme_void() +
    (if (isTRUE(polar)) ggplot2::coord_polar() else ggplot2::coord_fixed()) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = background_color, color = NA),
      plot.background = ggplot2::element_rect(fill = background_color, color = NA)
    )
  dir.create(img_path, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(img_path, file_name)
  ggplot2::ggsave(plot, filename = path, width = width, height = height, device = filetype)
  message("image saved: ", path)
  invisible(path)
}

# make R CMD check aware of the column names used in aes()
utils::globalVariables(c("x", "y", "seed"))
