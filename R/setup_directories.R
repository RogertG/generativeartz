#' Setup a directory structure for image creation
#'
#' I use a certain directory structure for image creation. Every image is saved under "img/everything/". I use the directory "img/handpicked/" for a selection of good images. Moreover I create a directory for the log file.
#' You only need to call this function one time.
#' @param img_path the path, where the images should be saved
#' @param img_subdir one subdirectory
#' @param img_subdir2 a second subdirectory
#' @param logfile_dir the log file directory
#' @return the created directories (invisibly)
#' @export
#' @examples
#' \dontrun{
#' setup_directories("img/", "everything/", "handpicked/", "logfile/")
#' }

setup_directories <- function(img_path, img_subdir, img_subdir2, logfile_dir) {
  dirs <- c(file.path(img_path, img_subdir), file.path(img_path, img_subdir2), logfile_dir)
  for (d in dirs) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  invisible(dirs)
}
