box::use(
  shiny[...],
  shiny.semantic,
  shinyjs,
  purrr,
  glue[glue],
  app / view / pickStock,
  app / view / dynamicTabs,
  readr
)

sp500nms <- readr$read_rds("app/logic/sp500nms.rds")

stock_limit <- 10

#' @export
ui <- function(id) {
  ns <- NS(id)
  shiny.semantic$semanticPage(
    shinyjs$useShinyjs(),
    shiny.semantic$sidebar_layout(
      shiny.semantic$sidebar_panel(
        pickStock$ui(ns("picker"))
      ),
      shiny.semantic$main_panel(
        dynamicTabs$ui(ns("dynamic_tabs"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    tickers_selected <- reactiveValues()
    
    for (i in 1:stock_limit) {
      tickers_selected[[glue("ticker_{i}")]] <- NULL
    }
    
    pickStock$server(
      id          = "picker",
      choices     = sp500nms,
      selection   = tickers_selected,
      stock_limit = stock_limit
    )
    
    dynamicTabs$server(
      id = "dynamic_tabs",
      tickers_selected = tickers_selected
    )
  })
}
