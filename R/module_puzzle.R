puzzle_ui <- function(id) {
  ns <- NS(id)
  tagList(

    column(
      width = 12,
      align = "center",
      div(textInput(ns("word"),
                    "Guess a 5-letter word",
                    width = "90%"),
          style = "display:inline-block"),
      div(actionBttn(ns("enter"),
                     "Enter"),
          style = "display:inline-block"),
      plotOutput(ns("puzzle")),
      plotOutput(ns("keyboard"),
                 width = "70%")
    )

  )
}

puzzle_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    # set.seed(19841013) for testing
    # prep candidate and answer words
    candidates <- reactiveVal("")
    answer <- reactiveVal(sample(dict, 1))

    ## logic when enter is pressed
    observeEvent(input$enter, {

      # browser()
      ns <- session$ns
      answer <- answer()

      # checking the input word contains only 5 letters
      if (str_length(input$word) != 5 ) {
        showModal(modalDialog(
          "Input word must contain 5 letters",
          easyClose = TRUE
        ))
      } else if (!input$word %in% dict) {

        showModal(modalDialog(
          "It's not a valid word",
          easyClose = TRUE
        ))

      } else {

        update_words <- c(candidates(), input$word)
        update_words <- update_words[update_words != ""]
        candidates(update_words)

        # BINGO
        if (input$word == answer) {
          showModal(modalDialog(
            "Good Job!",
            footer = actionBttn(ns("nxt"), "Next")
          ))

        }
        # Used all guesses
        if (length(candidates()) == 6 && !answer %in% candidates()) {
          showModal(modalDialog(
            HTML(glue::glue("Answer is <strong>{answer}</strong>.
                       <br> Better luck next time!")),
            footer = actionBttn(ns("nxt"), "Next")
          ))
        }
      }

      # refresh input text
      updateTextInput(inputId = "word",
                      value = "")
    })

    ## logic when next is pressed
    observeEvent(input$nxt, {

      removeModal()

      # update candidates
      candidates("")

      # refresh answer
      answer(sample(dict, 1))

    })

    ## build puzzle plot
    puzzle_plot <- reactive({

      # browser()
      # plot puzzle
      build_puzzle_df(candidates(), answer()) %>%
        plot_puzzle()

    })

    # render puzzle plot
    output$puzzle <- renderPlot({

      puzzle_plot()

    })

    # render keyboard plot
    keyboard_plot <- reactive({

      build_keyboard_df(candidates(), answer()) %>%
        plot_keyboard()

    })

    output$keyboard <- renderPlot({

      keyboard_plot()

    })


  })

}
