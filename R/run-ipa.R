#' Run IPA Random Forest
#'
#' Launch shiny application.
#' @importFrom shiny runApp
#' @export
run_ipa <- function() {
  app_dir <- system.file("ipa", package = "tidyds")
  shiny::runApp(app_dir, display.mode = "normal")
}


