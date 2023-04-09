# styler: off
box::use(
  shiny[...],
  shiny.semantic,
  stringr,
  quantmod,
  PerformanceAnalytics,
  shinycssloaders,
  app/logic/plots[plot_dev_ret]
)
# styler: on

#' UI riskmod module
#'
#' @param id Module's id
#' @export
#' @name risk-module
ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny.semantic$flow_layout(
      shiny.semantic$card(
        div(class = "content", div(class = "header", textOutput(ns("sd"))))
      ),
      shiny.semantic$card(
        div(class = "content", div(class = "header", textOutput(ns("ret"))))
      )
    ),
    plotOutput(ns("deviations_plot")) |>
      shinycssloaders$withSpinner(type = 8, color = "gray")
  )
}


#' Server function for riskmod module.
#'
#' @param id a name for the id
#' @param returns_data \code{xts} containing one column of returns
#'
#' @export
#' @rdname risk-module
server <- function(id, returns_data) {
  stopifnot(is.reactive(returns_data))
  moduleServer(id, function(input, output, session) {
    # returns
    ret <- reactive({
      quantmod$Ad(returns_data()) |>
        PerformanceAnalytics$Return.calculate(method = "log")
    })

    # standard deviation
    std <- reactive({
      ret() |>
        PerformanceAnalytics$StdDev()
    })

    output$deviations_plot <- renderPlot({
      plot_dev_ret(ret(), std())
    })


    output$ret <- renderText({
      stringr$str_c("Average Return: ", round(mean(ret() * 100, na.rm = TRUE), 5), "%")
    })

    output$sd <- renderText({
      stringr$str_c("Standard Deviation: ", round(std() * 100, 3), "%")
    })
  })
}
