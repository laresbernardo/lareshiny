####################################################################
try_require2 <- function(package, stop = TRUE, load = TRUE, lib.loc = NULL, ...) {
  present <- length(find.package(package, quiet = TRUE)) > 0
  if (present && load) {
    # Be careful: ... parameter is not enabled in library()
    suppressPackageStartupMessages(library(package, character.only = TRUE, lib.loc = lib.loc))
  } else {
    if (stop) {
      stop(paste0("Package '", package, "' required. Install and try again."), call. = FALSE)
    } else {
      warning(paste0("Package '", package, "' recommended. Install for better results."), call. = FALSE)
    }
  }
}
