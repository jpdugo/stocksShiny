box::use(
  shiny[...],
  purrr,
  shiny.semantic,
  app / view / tickerInfo,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tabs"))
  )
}

#' @export
server <- function(id, tickers_selected) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(tickers_selected |> reactiveValuesToList(), {
        tickers_selected |>
          reactiveValuesToList() |>
          purrr$keep(\(x) !is.null(x)) |>
          purrr$map(\(x) tickerInfo$server(x, reactive(x)))
      })

      output$tabs <- renderUI({
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
    }
  )
}
