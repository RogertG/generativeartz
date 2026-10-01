#' Check, if a logfile exists
#'
#' This function loads the logfile, if it exists. If it doesn't, it returns an empty log
#' with the expected columns.
#' @param logfile_path path of the log file. Defaults to the global variable `LOGFILE_PATH`
#'   if it exists, otherwise `"logfile/logfile.csv"`.
#' @return a data frame
#' @seealso \code{\link{generate_logfile_entry}} get the data to be logged
#' @export
#' @examples
#' check_logfile_existence(tempfile(fileext = ".csv"))

check_logfile_existence <- function(logfile_path = default_logfile_path()) {
  if (file.exists(logfile_path)) {
    message("load logfile")
    logfile <- utils::read.delim(logfile_path, stringsAsFactors = FALSE)
    # older versions of the package wrote an all-NA first row
    logfile <- logfile[!is.na(logfile$file_name), , drop = FALSE]
  } else {
    message("create logfile")
    logfile <- empty_logfile()
  }
  logfile
}
