#' UI for pickStock module
#' 
#' Make a manual or random pick from a selectizeInput containing some choices
#'
#' @param id a name to supply to \code{NS()}
#'
#' @return taglist
#' @import shiny
#' @export
#'
pickStockUI <- function(id) {
  tagList(
    selectizeInput(NS(id, "tickers"), "Select S&P500 Stocks", multiple = TRUE, choices = ""),
    shiny.semantic::action_button(NS(id, "reset_rand_counter"), "Reset Picks"),
    br(),
    shiny.semantic::checkbox_input(NS(id, "show_rand"), "Random Pick", is_marked = FALSE),
    shinyjs::hidden(
      div(
        id = NS(id, "random_pick_menu"),
        div(class = "ui green button", id = NS(id, "counter"), "Make a Random Peak"),
        shiny.semantic::numeric_input(NS(id, "n_rand_picks"), "N' of Picks", value = 1, min = 1, max = 10)
      )
    ),
    div(id = NS(id, "get_data"), class = "ui teal button", "Get Data")
  )
}

#' Server function for pickStock module
#'
#' @param id The server id
#' @param choices A character vector for the choices
#'
#' @return a reactive character vector
#' 
#' @import shiny
#' @export
#'
#' @examples
#'pickStockServer('my_id', c('choice1', 'choice2'))
pickStockServer <- function(id, choices) {
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
          shinyjs::show("random_pick_menu")
        } else {
          shinyjs::hide("random_pick_menu")
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
      shinyjs::enable("tickers")
    })

    observeEvent(input$tickers, {
      if (length(input$tickers) >= 10) {
        shinyjs::disable("tickers")
      }
    })

    eventReactive(input$get_data, {
      input$tickers
    })
  })
}
