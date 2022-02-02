tickerInfoUI <- function(id) {
  tagList(
    br(),
    date_input(NS(id, "date"), "From",
                   min = "2001-01-01",
                   max = "2022-12-21",
                   value = '2021-01-01'
              )
    
  )
}

tickerInfoServer <- function(id, ticker) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(getSymbols(ticker, auto.assign = FALSE, from = input$date))
  })
}


