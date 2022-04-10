#' UI riskmod module
#'
#' @param id a name for the id
#' 
#' @import shiny
#' 
#' @return taglist
#' @export
#'
riskmodUI <- function(id) {
  tagList(
    shiny.semantic::flow_layout(
      shiny.semantic::card(
        div(
          class = "content",
          div(class = "header", textOutput(NS(id, "sd"))),
        )
      ),
      shiny.semantic::card(
        div(
          class = "content",
          div(class = "header", textOutput(NS(id, "ret"))),
        )
      )
    ),
    plotOutput(NS(id, "deviations_plot"))
  )
}


#' Server function for riskmod module.
#'
#' @param id a name for the id
#' @param returns_data an xts object containing one column of returns
#'
#' @import shiny
#' @export
riskmodServer <- function(id, returns_data) {
  stopifnot(is.reactive(returns_data))
  moduleServer(id, function(input, output, session) {

    # returns
    ret <- reactive({
      quantmod::Ad(returns_data()) %>%
        PerformanceAnalytics::Return.calculate(method = "log")
    })

    # standard deviation
    std <- reactive({
      ret() %>%
        PerformanceAnalytics::StdDev()
    })

    output$deviations_plot <- renderPlot({
      plot_dev_ret(ret(), std())
    })


    output$ret <- renderText({
      stringr::str_c("Average Return: ", round(mean(ret() * 100, na.rm = TRUE), 5), "%")
    })

    output$sd <- renderText({
      stringr::str_c("Standard Deviation: ", round(std() * 100, 3), "%")
    })
  })
}