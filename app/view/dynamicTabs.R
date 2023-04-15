box::use(
  shiny[...],
  purrr
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tabs"))
  )
}

#' @export
server <- function(id, nms) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      spliced_nms <- reactiveValues()
      
      observeEvent(nms(), {
        purrr$walk(
          nms(),
          \(x) {
            spliced_nms[[x]] <- x
          }
        )
      })
      

      output$tabs <- renderUI(
        spliced_nms |> reactiveValuesToList()
      )
    }
  )
}