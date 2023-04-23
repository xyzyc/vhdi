#' Run example for the vhdi package
#'
#' @export
#' @examples
#' \dontrun{
#' runExample()
#' }
runExample <- function() {
  appDir <- system.file("shiny-examples", "myapp",
                        package = "vhdi")
  if (appDir == "") {
    stop(paste0("Could not find example directory. ",
                "Try re-installing `mypackage`."), call. = FALSE)
  }
  # the first app will be called
  shiny::runApp(appDir[1], display.mode = "normal")
}
