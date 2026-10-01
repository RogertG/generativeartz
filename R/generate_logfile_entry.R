#' Append an entry to the logfile
#'
#' This function saves the information needed to recreate an image with random elements
#' (seed, formula and all data and plot parameters) and appends it to the logfile.
#' @param logfile the logfile, as returned by `check_logfile_existence()`
#' @param formula the formula list that is used
#' @param seed the specific seed used
#' @param file_name the file name of the image
#' @param params named list of the data and plot parameters (see `generate_img()`).
#'   Missing values are filled with the package defaults.
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return the updated log data frame (invisibly); the log file is written as a side effect
#' @seealso \code{\link{check_logfile_existence}} check, if there is a logfile
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' formula <- list(x = quote(sin(x_i)), y = quote(cos(y_i)))
#' generate_logfile_entry(check_logfile_existence(path), formula, 42, "img.png",
#'                        logfile_path = path)
#' @importFrom dplyr bind_rows

generate_logfile_entry <- function(logfile, formula, seed, file_name, params = list(),
                                   logfile_path = default_logfile_path()) {
  params <- resolve_params(params)
  params$width <- NULL
  params$height <- NULL
  logfile_tmp <- data.frame(file_name = file_name,
                            seed = seed,
                            formula_x = paste(deparse(formula[["x"]]), collapse = " "),
                            formula_y = paste(deparse(formula[["y"]]), collapse = " "),
                            params,
                            stringsAsFactors = FALSE)
  logfile <- dplyr::bind_rows(logfile, logfile_tmp)
  dir.create(dirname(logfile_path), showWarnings = FALSE, recursive = TRUE)
  # write doubles with full precision so that e.g. 2 * pi is restored exactly
  out <- logfile
  is_dbl <- vapply(out, is.double, logical(1))
  out[is_dbl] <- lapply(out[is_dbl], function(v) ifelse(is.na(v), NA, sprintf("%.17g", v)))
  utils::write.table(out, logfile_path, sep = "\t", quote = unname(which(!is_dbl)), row.names = FALSE)
  message("logfile saved")
  invisible(logfile)
}
