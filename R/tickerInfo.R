tickerInfoUI <- function(id) {
  tagList(
    br(),
    date_input(NS(id, "date"), "From: ",
      min = "2001-01-01",
      max = "2022-12-21",
      value = "2021-12-31",
      style = "width: 250px;"
    ),
    plotlyOutput(NS(id, "candle")),
    br(),
    htmlOutput(NS(id, "finviz_table"))
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
      data_df() %>% 
        candlestick_plotly(id)
    })

    output$finviz_table <- renderText({
      x <- kable(get_table_finviz(id), format = "html", caption = 'Finviz Table:') %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12)
      gsub("<thead>.*</thead>", "", x)
    })
  })
}
