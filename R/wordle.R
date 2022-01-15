#' The wordle app
#'
#' @return Shiny app
#' @export
#'
#' @import shiny
#' @importFrom shinyWidgets actionBttn
#'
wordleApp <- function() {

  shinyApp(
    ui = fluidPage(
      title = "WORDLE-Shiny",
      header_ui("header"),
      puzzle_ui("puzzle")
    ),
    server = function(input, output, session) {
      header_server("header")
      puzzle_server("puzzle")

    }
  )
}
