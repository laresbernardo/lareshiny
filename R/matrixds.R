####################################################################
#' MatrixDS Auto Working Directory for Shiny
#' 
#' This function automatically detects which of the Shiny paths 
#' should be used when running a Shiny App on MatrixDS (internal
#' RStudio session or external production version). It also
#' works for a local file so the script is the same.
#' 
#' @param shiny_project Character. Directory's name
#' @export
matrixwd <- function(shiny_project = "Shiny") {
  prod <- paste0("/srv/shiny-server/", shiny_project)
  dev <- paste0("~/shiny-server/", shiny_project)
  if (!dir.exists(prod) & !dir.exists(dev)) {
    shiny_path <- getwd()
  } else {
    .libPaths(c(.libPaths(), "/srv/.R/library"))
    options(java.parameters = "-Xmx8048m")
    if (dir.exists(prod)) shiny_path <- prod
    if (dir.exists(dev)) shiny_path <- dev
  }  
  return(shiny_path)
}
