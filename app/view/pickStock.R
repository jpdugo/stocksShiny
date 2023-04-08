box::use(
  shiny,
  shiny.semantic,
  shinyjs
)


#' UI for pickStock module
#'
#' Pick a stock from a selectizeInput containing s&p500 tickers
#'
#' @param id
#'
#' @return taglist
#' @export
#'
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$selectizeInput(
      inputId  = ns("tickers"),
      label    = "Select S&P500 Stocks",
      multiple = TRUE,
      choices  = ""
    ),
    shiny.semantic$action_button(
      input_id = ns("reset_rand_counter"),
      label    = "Reset Picks"
    ),
    shiny$br(),
    shiny$div(
      shiny.semantic$checkbox_input(
        input_id  = ns("show_rand"),
        label     = "Random Pick",
        is_marked = FALSE
      )
    ),
    shinyjs$hidden(
      shiny$div(
        id = ns("random_pick_menu"),
        shiny$div(
          class = "ui green button",
          id = ns("counter"),
          "Make a Random Peak"
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
    shiny$div(
      id = ns("get_data"),
      class = "ui teal button",
      "Get Data"
    )
  )
}

#' Server function for pickStock module
#'
#' @param id
#' @param choices \code{character} choices
#'
#' @return \code{reactive} character vector
#'
#' @import shiny
#' @export
server <- function(id, choices) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$observe(
      {
        shiny$updateSelectizeInput(inputId = "tickers", choices = choices)
      },
      autoDestroy = TRUE
    )

    shiny$observeEvent(input$show_rand,
      {
        if (input$show_rand) {
          shinyjs$show("random_pick_menu")
        } else {
          shinyjs$hide("random_pick_menu")
        }
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(input$counter,
      {
        samp <- sample(choices, input$n_rand_picks)
        shiny$updateSelectizeInput(session, "tickers", selected = samp)
      },
      ignoreInit = TRUE
    )


    shiny$observeEvent(input$reset_rand_counter, {
      shiny$updateSelectizeInput(session, "tickers", selected = "")
      shinyjs$enable("tickers")
    })

    shiny$observeEvent(input$tickers, {
      if (length(input$tickers) >= 10) {
        shinyjs$disable("tickers")
      }
    })

    shiny$eventReactive(input$get_data, {
      input$tickers
    })
  })
}
