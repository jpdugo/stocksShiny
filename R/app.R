#' Run stock application.
#'
#' This application let's the user download information about sp500 tickers and
#' take a look at different metrics and indicators.
#' 
#' @param ... Additional arguments to pass to \code{shinyApp()}
#' 
#' @import shiny
#' @importFrom purrr map
#'
#' @export
run_app <- function(...) {
  ui <- shiny.semantic::semanticPage(
    shinyjs::useShinyjs(),
    shiny.semantic::sidebar_layout(
      shiny.semantic::sidebar_panel(
        pickStockUI("picker")
      ),
      shiny.semantic::main_panel(uiOutput("stock_tabs"))
    )
  )
  server <- function(input, output, session) {
    ticker <- pickStockServer("picker", sp500nms)

    observeEvent(ticker(), {
      output$stock_tabs <- renderUI({
        shiny.semantic::tabset(tabs = ticker() %>%
          map(~ list(menu = .x, content = tickerInfoUI(.x))))
      })

      for (i in ticker()) {
        tickerInfoServer(i)
      }
    })
  }
  shinyApp(ui, server, ...)
}
