box::use(
  shiny[...],
  shiny.semantic,
  shinyjs
)

#' @title pickStock
#'
#' @description Pick a stock from a selectizeInput containing s&p500 tickers
#'
#' @param id Module's id.
#' @return a [shiny::reactive()] function containing a character vector.
#' @export
#'
#' @name pickStock-module
ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId  = ns("tickers"),
      label    = "Select S&P500 Stocks",
      multiple = TRUE,
      choices  = ""
    ),
    shiny.semantic$action_button(
      input_id = ns("reset_rand_counter"),
      label    = "Reset Picks"
    ),
    br(),
    div(
      shiny.semantic$checkbox_input(
        input_id  = ns("show_rand"),
        label     = "Random Pick",
        is_marked = FALSE
      )
    ),
    shinyjs$hidden(
      div(
        id = ns("random_pick_menu"),
        div(
          class = "ui green button",
          id = ns("counter"),
          "Make a Random Pick"
        ),
        shiny.semantic$numeric_input(
          input_id = ns("n_rand_picks"),
          label    = "N' of Picks",
          value    = 1,
          min      = 1,
          max      = 10
        )
      )
    ),
    div(
      id = ns("get_data"),
      class = "ui teal button",
      "Get Data"
    )
  )
}


#' @export
#'
#' @rdname tickerInfo-module
server <- function(id, choices) {
  moduleServer(id, function(input, output, session) {
    observe(
      {
        updateSelectizeInput(inputId = "tickers", choices = choices)
      },
      autoDestroy = TRUE
    )

    observeEvent(input$show_rand,
      {
        if (input$show_rand) {
          shinyjs$disable("tickers")
          shinyjs$show("random_pick_menu")
        } else {
          shinyjs$enable("tickers")
          shinyjs$hide("random_pick_menu")
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
      shinyjs$enable("tickers")
    })

    observeEvent(input$tickers, {
      if (length(input$tickers) >= 10) {
        shinyjs$disable("tickers")
      }
    })

    eventReactive(input$get_data, {
      input$tickers
    })
  })
}
