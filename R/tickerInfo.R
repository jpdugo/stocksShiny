tickerInfoUI <- function(id) {
  tagList(
    
  )
}

tickerInfoServer <- function(id, ticker) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(getSymbols(ticker, auto.assign = FALSE, from = input$date))
  })
}