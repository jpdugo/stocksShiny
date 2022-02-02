stocksInfo <- function(...) {
library(shiny)
library(quantmod)
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
  stocks_picked <- pickStockServer("picker", sp500nms)

  observeEvent(stocks_picked(),{
    output$stock_tabs <- renderUI({
      tabset(tabs = stocks_picked() %>% map(~ list(menu = .x, content = .x)))
    })
  })
  
  
}
shinyApp(ui, server, ...)
}
