#' UI for tickerInfo module.
#'
#' @param id a name for the id
#'
#' @return taglist
#' 
#' @importFrom magrittr %>%
#' @import shiny
#
#' @export
tickerInfoUI <- function(id) {
  tagList(
    shiny.semantic::tabset(
      list(
        # First tab
        list(
          menu = "Overview", content =
            tagList(
              br(),
              shiny.semantic::date_input(NS(id, "date"), "From: ",
                min = "2001-01-01",
                max = "2022-12-21",
                value = "2018-12-31",
                style = "width: 250px;"
              ),
              shiny.semantic::selectInput(NS(id, "period"), "Periodicity",
                choices = c("days", "weeks", "months", "quarters"),
                selected = "days"
              ),
              shiny.semantic::selectInput(NS(id, "index_at"), "indexAt",
                choices = c("firstof", "lastof", "startof", "endof"),
                selected = "endof"
              ),
              br(),
              plotly::plotlyOutput(NS(id, "candle")) %>% shinycssloaders::withSpinner(),
              htmlOutput(NS(id, "finviz_table")) %>% shinycssloaders::withSpinner()
            )
        ),
        # Second Tab
        list(menu = "Returns", content = returnsUI(NS(id, "returns"))),
        # third Tab
        list(menu = "Risk", content = riskmodUI(NS(id, "risk")))
      )
    )
  )
}

#' Server for tickerInfoServer
#'
#' @param id a name for the id
#'
#' @return
#' @import shiny
#' @export
tickerInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$date)
      req(input$index_at)
      req(input$period)
      quantmod::getSymbols(id, auto.assign = FALSE, from = input$date)
    })



    data_period <- reactive({
      # to avoid unnecessary requests
      xts::to.period(data(), period = input$period, indexAt = input$index_at, OHLC = FALSE)
    })

    data_df <- reactive(xts_to_tibble(data_period()))

    output$candle <- plotly::renderPlotly({
      data_df() %>%
        candlestick_plotly(id)
    })

    output$finviz_table <- renderText({
      get_table_finviz(id) %>%
        no_thead_kable()
    })

    returnsServer("returns", data_period)
    riskmodServer("risk", data_period)
  })
}
