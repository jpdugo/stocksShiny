stocksInfo <- function(...) {
library(shiny)
library(shiny.semantic)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)

ui <- semanticPage(
  useShinyjs(),
  sidebar_layout(
    sidebar_panel(
      pickStockUI("picker")
    ),
    main_panel(uiOutput('stock_tabs'))
  )
)
server <- function(input, output, session) {
  stocks_picked <- pickStockServer("picker")

  observeEvent(stocks_picked(),{
    print(stocks_picked())
  })
  
  
}
shinyApp(ui, server, ...)
}
