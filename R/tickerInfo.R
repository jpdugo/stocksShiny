tickerInfoUI <- function(id) {
  tagList(
    tabset(
      list(
        #First tab
        list(
          menu = "Overview", content =
            tagList(
              br(),
              date_input(NS(id, "date"), "From: ",
                min = "2001-01-01",
                max = "2022-12-21",
                value = "2018-12-31",
                style = "width: 250px;"
              ),
              selectInput(NS(id, "period"), "Periodicity",
                          choices = c("days", "weeks", "months", "quarters"),
                          selected = "days"),
              selectInput(NS(id, "index_at"), "indexAt",
                          choices = c("firstof", "lastof", "startof", "endof"),
                          selected = "endof"),
              plotlyOutput(NS(id, "candle")),
              htmlOutput(NS(id, "finviz_table"))
            )
        ),
        #Second Tab
        list(menu = 'Returns', content = returnsUI(NS(id, 'returns'))) #NS goes here too
      )
    )
  )
}

tickerInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$date)
      req(input$index_at)
      req(input$period)
      getSymbols(id, auto.assign = FALSE, from = input$date)
    })



    data_period <- reactive({
      # to avoid unnecessary requests
      to.period(data(), period = input$period, indexAt = input$index_at, OHLC = FALSE)
    })

    data_df <- reactive(xts_to_tibble(data_period()))

    output$candle <- renderPlotly({
      data_df() %>%
        candlestick_plotly(id)
    })

    output$finviz_table <- renderText({
      get_table_finviz(id) %>%
        no_thead_kable()
    })
    
    #Returns tab observers
    
    stock_returns <- returnsServer('returns', data_period)
    
    observe({print(stock_returns())})
  })
}
