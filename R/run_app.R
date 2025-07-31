#' Launch the Wine Explorer Shiny app
#'
#' @return No return value.
#' @examples
#' \dontrun{
#'   vinexplorer::run_app()
#' }
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "vinexplorer")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `vinexplorer`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
