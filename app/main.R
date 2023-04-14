box::use(
  shiny[...],
  shiny.semantic,
  shinyjs[useShinyjs],
  purrr,
  glue[glue],
  app / view / pickStock,
  app / view / tickerInfo
)

load("./app/logic/sp500nms.rda")

stock_limit <- 10

#' @export
ui <- function(id) {
  ns <- NS(id)
  shiny.semantic$semanticPage(
    useShinyjs(),
    shiny.semantic$sidebar_layout(
      shiny.semantic$sidebar_panel(pickStock$ui(ns("picker"))),
      shiny.semantic$main_panel(uiOutput(ns("stock_tabs")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tickers_selected <- reactiveValues()

    for (i in 1:stock_limit) {
      tickers_selected[[glue("ticker_{i}")]] <- NULL
    }

    pickStock$server(
      id          = "picker",
      choices     = sp500nms,
      selection   = tickers_selected,
      stock_limit = stock_limit
    )

    observeEvent(tickers_selected |> reactiveValuesToList(), {
      tickers_selected |>
        reactiveValuesToList() |>
        purrr$keep(\(x) !is.null(x)) |>
        purrr$map(\(x) tickerInfo$server(x, reactive(x)))
    })

    output$stock_tabs <- renderUI({
      shiny.semantic$tabset(
        tabs = tickers_selected |>
          reactiveValuesToList() |>
          purrr$keep(\(x) !is.null(x)) |>
          purrr$map(
            \(x) list(
              menu = x,
              content = tickerInfo$ui(ns(x))
            )
          )
      )
    })
  })
}
