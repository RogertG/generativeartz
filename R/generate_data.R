#' Generate data
#'
#' The generative images are based on values in a dataframe. This function creates a grid of
#' all combinations of `x_i` and `y_i` from `seq(range_from, range_to, by = step)` and
#' transforms it with a `formula`.
#' @param formula a list that contains formulas for transforming the x- and y-values.
#'   The formulas can use the variables `x_i` and `y_i`.
#' @param range_from,range_to start and end of the base sequence. Default is `-2 * pi` to `2 * pi`.
#' @param step step size of the base sequence. Smaller steps give more points (the number of
#'   points grows quadratically) and therefore denser images, but take longer.
#' @return data frame
#' @seealso \code{\link{generate_plot}} the returned data frame is the input to generate the plot
#' @export
#' @examples
#' formula <- list(
#'   x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
#'   y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
#' )
#' df <- generate_data(formula, step = 0.1)
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%

generate_data <- function(formula, range_from = -2 * pi, range_to = 2 * pi, step = 0.01) {
  validate_formula(formula)
  message("generate data")
  base <- seq(from = range_from, to = range_to, by = step)
  expand.grid(x_i = base, y_i = base) %>%
    dplyr::mutate(!!!formula[c("x", "y")])
}
