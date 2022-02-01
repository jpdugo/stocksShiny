pickStockUI <- function(id) {
  tagList(
    selectizeInput(NS(id, "tickers"), "Select S&P500 Stocks", multiple = TRUE, choices = ''),
    action_button(NS(id, "reset_rand_counter"), "Reset Picks"),
    br(),
    checkbox_input(NS(id, "show_rand"), "Random Pick", is_marked = FALSE),
    hidden(
      div(
        id = NS(id, "random_pick_menu"),
        div(class = "ui green button", id = NS(id, "counter"), "Make a Random Peak"),
        numeric_input(NS(id, "n_rand_picks"), "N' of Picks", value = 1, min = 1, max = 10)
      )
    ),
    br(),
    div(id = NS(id, "get_data"), class = "ui teal button", "Get Data")
  )
}

pickStockServer <- function(id, choices) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectizeInput(inputId = 'tickers', choices = choices)
    },autoDestroy = TRUE)
    
    observeEvent(input$show_rand,
      {
        if (input$show_rand) {
          show("random_pick_menu")
        } else {
          hide("random_pick_menu")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$counter,
      {
        samp <- sample(choices, input$n_rand_picks)
        updateSelectizeInput(session, "tickers", selected = samp)
      },
      ignoreInit = TRUE
    )


    observeEvent(input$reset_rand_counter, {
      updateSelectizeInput(session, "tickers", selected = "")
      enable("tickers")
    })

    observeEvent(input$tickers, {
      if (length(input$tickers) >= 10) {
        disable("tickers")
      }
    })

    eventReactive(input$get_data, {
      input$tickers
    })
  })
}
