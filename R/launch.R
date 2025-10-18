####################################################################
#' Launch Rosa App
#'
#' Launch Rosa's UI from any R session.
#'
#' @param ... Additional parameters
#' @export
launch <- function(...) {
  shinyApp(ui(), server)
}
