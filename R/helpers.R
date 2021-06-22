####################################################################
#' Check if Specific Package is Installed
#' 
#' This function checks library dependencies
#' 
#' @param package Character. Name of the library
#' @param stop Boolean. Stop if not installed. If \code{FALSE} and 
#' library is not available, warning will be shown.
#' @return No return value, called for side effects.
#' @examples 
#' # Check if library base is installed. If not, stop and show error
#' try_require("base", stop = TRUE)
#' # Check if library xxx is installed. If not, show warning
#' try_require("xxx", stop = FALSE)
#' @export
try_require <- function(package, stop = TRUE) {
  present <- length(find.package(package, quiet = TRUE)) > 0
  if (present) {
    suppressMessages(library(package, character.only = TRUE))
  } else {
    if (stop) {
      stop(paste0("Package '", package, "' required. Install and try again."), call. = FALSE) 
    } else {
      warning(paste0("Package '", package, "' recommended. Install for better results."), call. = FALSE)  
    }
  } 
}
