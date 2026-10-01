#' Get one or several seeds from the logfile
#'
#' @param seed_to_recreate the seed (or seeds) of the original image(s)
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return a vector of the seeds that are found in the log file
#' @seealso \code{\link{get_formula_from_logfile}} get the formula for recreation
#' @seealso \code{\link{regenerate_img}} function to recreate an image
#' @export
#' @examples
#' \dontrun{
#' get_seed_from_logfile(104)
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr pull

get_seed_from_logfile <- function(seed_to_recreate, logfile_path = default_logfile_path()) {
  check_logfile_existence(logfile_path) %>%
    dplyr::filter(seed %in% seed_to_recreate) %>%
    dplyr::pull(seed) %>%
    unique()
}
