riskmodUI <- function(id) {
  tagList(
    flow_layout(
      card(
        div(
          class = "content",
          div(class = "header", textOutput(NS(id, "sd"))),
        )
      ),
      card(
        div(
          class = "content",
          div(class = "header", textOutput(NS(id, "ret"))),
        )
      )
    ),
    plotOutput(NS(id, "deviations_plot"))
  )
}


riskmodServer <- function(id, returns_data) {
  stopifnot(is.reactive(returns_data))
  moduleServer(id, function(input, output, session) {

    # returns
    ret <- reactive({
      Ad(returns_data()) %>%
        Return.calculate(method = "log")
    })

    # standard deviation
    std <- reactive({
      ret() %>%
        StdDev()
    })

    output$deviations_plot <- renderPlot({
      plot_dev_ret(ret(), std())
    })


    output$ret <- renderText({
      str_c("Average Return: ", round(mean(ret() * 100, na.rm = TRUE), 5), "%")
    })

    output$sd <- renderText({
      str_c("Standard Deviation: ", round(std() * 100, 3), "%")
    })
  })
}