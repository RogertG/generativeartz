#' Get a formula from the logfile
#'
#' Extracts the formula of an image from the log file in order to be able to recreate the image.
#' If the seed was used several times, the most recent entry is used.
#' @param seed_to_recreate the seed of the original image
#' @param filetype unused, kept for backwards compatibility
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return a list containing the formula
#' @seealso \code{\link{get_seed_from_logfile}} get the seed, which is used as input
#' @seealso \code{\link{regenerate_img}} function to recreate an image
#' @export
#' @examples
#' \dontrun{
#' get_formula_from_logfile(104)
#' }

get_formula_from_logfile <- function(seed_to_recreate, filetype = NULL,
                                     logfile_path = default_logfile_path()) {
  entry <- get_logfile_entry(seed_to_recreate, logfile_path)
  list(
    x = parse(text = entry$formula_x)[[1]],
    y = parse(text = entry$formula_y)[[1]]
  )
}

# Return the most recent log entry for a seed as a one-row data frame.
get_logfile_entry <- function(seed_to_recreate, logfile_path) {
  if (length(seed_to_recreate) != 1) {
    stop("`seed_to_recreate` must be a single seed.", call. = FALSE)
  }
  logfile <- check_logfile_existence(logfile_path)
  entries <- logfile[logfile$seed == seed_to_recreate, , drop = FALSE]
  if (nrow(entries) == 0) {
    stop("Seed ", seed_to_recreate, " not found in the log file ", logfile_path, call. = FALSE)
  }
  formulas <- unique(paste(entries$formula_x, entries$formula_y))
  if (length(formulas) > 1) {
    warning("Seed ", seed_to_recreate, " was used with ", length(formulas),
            " different formulas; using the most recent entry.", call. = FALSE)
  }
  entries[nrow(entries), , drop = FALSE]
}
