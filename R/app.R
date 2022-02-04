stocksInfo <- function(...) {

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
  ticker <- pickStockServer("picker", sp500nms)

  observeEvent(ticker(),{
    output$stock_tabs <- renderUI({
      tabset(tabs = ticker() %>%
               map(~ list(menu = .x, content = tickerInfoUI(.x))))
    })
    
    for (i in ticker()) {
      tickerInfoServer(i)
    }
  })
  
  
}
shinyApp(ui, server, ...)
}
