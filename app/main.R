box::use(
  shiny[
    bootstrapPage, moduleServer, NS, renderText, tags, textOutput, observeEvent, uiOutput, renderUI
  ],
  shiny.semantic,
  shinyjs[useShinyjs],
  purrr,
  app/view/pickStock
)

load("app/logic/sp500nms.rda")

#' @export
ui <- function(id) {
  ns <- NS(id)
  shiny.semantic$semanticPage(
    useShinyjs(),
    shiny.semantic$sidebar_layout(
      shiny.semantic$sidebar_panel(
        pickStock$ui(ns("picker"))
      ),
      shiny.semantic::main_panel(uiOutput(ns("stock_tabs")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ticker <- pickStock$server("picker", sp500nms)

    # observeEvent(ticker(), {
    #   output$stock_tabs <- renderUI({
    #     shiny.semantic$tabset(tabs = ticker() %>%
    #       purrr$map(~ list(menu = .x, content = tickerInfoUI(.x))))
    #   })
    # 
    #   for (i in ticker()) {
    #     tickerInfoServer(i)
    #   }
    # })
  })
}
