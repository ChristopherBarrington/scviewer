
#' Start scviewer
#' 
#' @param ... All arguments are ignored
#' 
#' @details Uses the `ui` and `server` functions defined in `scviewer`.
#' 
#' @importFrom shiny shinyApp
#' 
#' @export
#' 
run_scviewer <- function(...) {
  shinyApp(ui, server)
}
