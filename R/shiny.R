#' Launch shiny acreular interface
#'
#' \code{shiny_acreular} launches a shiny interface for browser based ACR/EULAR calculations.
#'
#' @param display.mode The display mode to be passed to \link[shiny]{runApp}
#' @return NULL
#' @examples
#' \dontrun{
#' shiny_acreular()
#' shiny_acreular(display.mode="normal")
#' }
#' @export
shiny_acreular <- function(display.mode = "normal") {
  pkgs <- c("shiny", "DT", "FSA", "ggplot2", "ggiraph", "ggiraphExtra", "mime", "parsedate", "PMCMRplus", "readxl", "shinycssloaders", "shinyWidgets")
  missing <- sapply(pkgs, function(x){!requireNamespace(x, quietly=TRUE)})
  if (any(missing)) {
    stop(paste("The following package(s) are required for shiny_acreular to work:",
               paste(pkgs[missing], collapse=", ")),
         call. = FALSE)
  }
  app_dir <- system.file("shiny", package = "acreular")
  shiny::runApp(app_dir, display.mode = display.mode)
}
