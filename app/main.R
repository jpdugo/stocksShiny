box::use(
  shiny[...],
  shiny.semantic,
  shinyjs,
  purrr,
  glue[glue],
  app / view / pickStock,
  app / view / tickerInfo,
  app / view / dynamicTabs,
  readr
)

sp500nms <- readr$read_rds("app/logic/sp500nms.rds")

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
    ns <- session$ns

    selection <- pickStock$server(id = "picker", choices = sp500nms)
    
    dynamicTabs$server(
      "dynamic_tabs",
      nms = selection
    )
  })
}
