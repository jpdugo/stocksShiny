box::use(
  shiny[...],
  dplyr[...],
  stringr,
  shiny.semantic,
  shinycssloaders,
  PerformanceAnalytics,
  ggplot2,
  app / logic / tables[xts_to_tibble],
)

#' UI for returns module
#'
#' @description Display information about a ticker
#'
#' @param id Module's id
#'
#' @export
#' @name returns-module
ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny.semantic$numeric_input(
      input_id = ns("binwidth"),
      label    =  "Select Binwidth",
      value    = .005,
      step     = .001
    ),
    plotOutput(ns("returns_hist")) |>
      shinycssloaders$withSpinner(type = 8, color = "gray"),
    plotOutput(ns("investment_evolution")) |>
      shinycssloaders$withSpinner(type = 8, color = "gray")
  )
}


#' @param returns_data
#'
#' @export
#' @rdname returns-module
server <- function(id, returns_data) {
  stopifnot(is.reactive(returns_data))
  moduleServer(id, function(input, output, session) {
    ret <- reactive({
      PerformanceAnalytics$Return.calculate(returns_data(), method = "log")
    })

    ret_df <- reactive({
      xts_to_tibble(ret())
    })

    # first plot

    adjusted <- reactive({
      stringr$str_subset(names(ret_df()), "\\.Adjusted$")
    })

    output$returns_hist <- renderPlot({
      adjusted <- stringr$str_subset(names(ret_df()), "\\.Adjusted$")

      ret_df() |>
        ggplot2$ggplot(ggplot2$aes(x = .data[[adjusted()]])) +
        ggplot2$geom_histogram(
          binwidth = input$binwidth,
          alpha    = .75,
          fill     = "cornflowerblue",
          color    = "black"
        ) +
        ggplot2$ggtitle(stringr$str_c("Histogram of ", names(ret_df())[[7]])) +
        ggplot2$xlab("Return")
    })

    # second plot

    output$investment_evolution <- renderPlot({
      xts_to_tibble(returns_data()) |>
        transmute(date, one_dollar = .data[[adjusted()]] / .data[[adjusted()]][[1]]) |>
        ggplot2$ggplot(ggplot2$aes(x = date, y = one_dollar)) +
        ggplot2$geom_line() +
        ggplot2$ylab("Value") +
        ggplot2$ggtitle("Evolution of $1 Investment")
    })
  })
}
