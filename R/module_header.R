header_ui <- function(id) {
  ns <- NS(id)
  tagList(
    column(
      width = 12,
      align = "center",
      div(h1("WORDLE"),
          style = "display:inline-block"),
      div(actionBttn(ns("help"),
                     "?",
                     color = "success",
                     size = "sm"),
          style = "display:inline-block")
    ),
    hr(style = "border:solid 1px #808080;")
  )
}

header_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # TODO: Write instruction

    observeEvent(input$help, {

      showModal(
        modalDialog(
          title = "HOW TO PLAY",
          instruction(),
          easyClose = TRUE
        )
      )

    })

  })

}


instruction <- function() {

  tagList(
    p("Guess the ", strong("WORDLE"), " in 6 tries."),
    p("Each guess must be a valid 5 letter word in lower case. Hit the enter button to submit."),
    p("After each guess, the color of the tiles will change to show how close your guess was to the word."),
    hr(),
    p(strong("Example")),
    img(src="www/example.png",
        width="250",
        height="50"),
    p("The letter ",
      strong("V"),
      " is in the word and in the correct spot."),
    p("The letters ",
      strong("A"),
      " and ",
      strong("E"),
      " are in the word but in the wrong spot."),
    p("The letters ",
      strong("G"),
      " and ",
      strong("U"),
      " are in the word but in the wrong spot."),
    p("P.S. The correct word in the example is ", strong("voela"), ".")

  )

}
