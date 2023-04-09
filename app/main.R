box::use(
  shiny[
    bootstrapPage, moduleServer, NS, renderText, tags, textOutput, observeEvent, uiOutput,
    renderUI, req
  ],
  shiny.semantic,
  shinyjs[useShinyjs],
  purrr,
  app / view / pickStock,
  app / view / tickerInfo
)

load("./app/logic/sp500nms.rda")

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
    ticker <- pickStock$server("picker", sp500nms)

    observeEvent(ticker(), {
      for (i in seq_along(ticker())) {
        name <- ticker()[i]
        tickerInfo$server(name, name)
      }
    })

    output$stock_tabs <- renderUI({
      req(ticker())
      shiny.semantic$tabset(
        tabs = ticker() |>
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
