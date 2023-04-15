# styler: off
box::use(
  shiny[...],
  shiny.semantic,
  shinyjs,
  lubridate,
  plotly,
  shinycssloaders,
  quantmod,
  xts,
  app/logic/tables[xts_to_tibble, get_table_finviz, no_thead_kable],
  app/logic/plots[candlestick_plotly],
  app/view/ returns,
  app/view/risk
)
# styler: on


#' @title tickerInfo
#'
#' @description Display information about a ticker
#'
#' @param id Module's id.
#' @export
#'
#' @name tickerInfo-module
ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny.semantic$tabset(
      list(
        # First tab
        list(
          menu = "Overview", content =
            tagList(
              br(),
              shiny.semantic$date_input(
                input_id = ns("date"),
                label    =  "From: ",
                min      = "2001-01-01",
                max      = lubridate$today(),
                value    = lubridate$make_date(lubridate$year(lubridate$today()), 1, 1),
                style    = "width: 250px;"
              ),
              shiny.semantic$selectInput(
                inputId  = ns("period"),
                label    =  "Periodicity",
                choices  = c("days", "weeks", "months", "quarters"),
                selected = "days"
              ),
              shiny.semantic$selectInput(
                inputId  = ns("index_at"),
                label    =  "indexAt",
                choices  = c("firstof", "lastof", "startof", "endof"),
                selected = "endof"
              ),
              br(),
              plotly$plotlyOutput(ns("candle")) |>
                shinycssloaders$withSpinner(type = 8, color = "gray"),
              htmlOutput(ns("finviz_table")) |>
                shinycssloaders$withSpinner(type = 8, color = "gray")
            )
        ),
        # Second Tab
        list(
          menu    = "Returns",
          content = returns$ui(ns("returns"))
        ),
        # third Tab
        list(
          menu = "Risk",
          content = risk$ui(ns("risk"))
        )
      )
    )
  )
}

#' @param id
#' @param ticker
#'
#' @export
#'
#' @rdname pickStock-module
server <- function(id, ticker) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$date, input$index_at, input$period)
      quantmod$getSymbols(
        Symbols     = ticker(),
        auto.assign = FALSE,
        from        = input$date
      )
    })

    data_period <- reactive({
      # use to.period to avoid unnecessary requests from getSymbols
      xts$to.period(
        x       = data(),
        period  = input$period,
        indexAt = input$index_at,
        OHLC    = FALSE
      )
    })

    data_df <- reactive(xts_to_tibble(data_period()))

    output$candle <- plotly$renderPlotly({
      data_df() |>
        candlestick_plotly(ticker())
    })

    output$finviz_table <- renderText({
      ticker() |>
        get_table_finviz() |>
        no_thead_kable()
    })

    returns$server("returns", data_period)
    risk$server("risk", data_period)
  })
}
