tickerInfoUI <- function(id) {
  tagList(
    br(),
    date_input(NS(id, "date"), "From: ",
      min = "2001-01-01",
      max = "2022-12-21",
      value = "2021-01-01",
      style = "width: 250px;"
    ),
    plotlyOutput(NS(id, "candle"))
  )
}

tickerInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$date)
      getSymbols(id, auto.assign = FALSE, from = input$date)
    })
    data_df <- reactive(xts_to_tibble(data()))

    output$candle <- renderPlotly({
      fig <- data_df() %>% plot_ly(
        x = ~date, type = "candlestick",
        open = as.formula(str_c('~', id, ".Open")), close = as.formula(str_c('~', id, ".Close")),
        high = as.formula(str_c('~', id, ".High")), low = as.formula(str_c('~', id, ".Low"))
      )
      fig %>% layout(title = id)
    })
  })
}
