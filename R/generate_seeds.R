#' Generate seeds
#'
#' This function creates a vector of distinct random numbers between 1 and 10000 that are used
#' to set the seed. Since every image needs its own seed, the number of images you want to
#' create equals the number of seeds.
#' @param nr_of_img a numeric input specifying the number of images that should be generated.
#' @return integer vector
#' @seealso \code{\link{generate_filename}} the file names are created based on the seed
#' @export
#' @examples
#' generate_seeds(3) # creates the seeds for three images

generate_seeds <- function(nr_of_img) {
  sample(1:10000, nr_of_img)
}
