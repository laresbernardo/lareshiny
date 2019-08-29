####################################################################
#' Check if Specific Package is Installed
#' 
#' This function checks library dependencies
#' 
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed
#' @export
try_require <- function(package, stop = TRUE) {
  if (length(find.package(package, quiet = TRUE)) > 0) {
    library(package, character.only = TRUE)
    return(invisible())
  }
  if (stop)
    stop(paste0("Package `", package, "` required. Install and try again."), call. = FALSE)
}
