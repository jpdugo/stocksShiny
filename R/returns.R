returnsUI <- function(id) {
  tagList(
    numeric_input(NS(id, "binwidth"), "Select Binwidth", value = .005, step = .001),
    plotOutput(NS(id, "returns_hist")),
    plotOutput(NS(id, "investment_evolution"))
  )
}

returnsServer <- function(id, returns_data) {
  stopifnot(is.reactive(returns_data))
  moduleServer(id, function(input, output, session) {
    ret <- reactive({
      Return.calculate(returns_data(), method = "log")
    })

    ret_df <- reactive({
      xts_to_tibble(ret())
    })

    # first plot

    adjusted <- reactive({
      str_subset(names(ret_df()), "\\.Adjusted$")
    })

    output$returns_hist <- renderPlot({
      adjusted <- str_subset(names(ret_df()), "\\.Adjusted$")

      ret_df() %>%
        ggplot(aes(x = .data[[adjusted()]])) +
        geom_histogram(binwidth = input$binwidth, alpha = .75, fill = "cornflowerblue", color = "black") +
        ggtitle(str_c('Histogram of ',names(ret_df())[[7]])) +
        xlab("Return")
    })

    # second plot

    output$investment_evolution <- renderPlot({
      xts_to_tibble(returns_data()) %>%
        transmute(date, one_dollar = .data[[adjusted()]] / .data[[adjusted()]][[1]]) %>%
        ggplot(aes(x = date, y = one_dollar)) +
        geom_line() +
        ylab('Value') +
        ggtitle('Evolution of $1 Investment')
    })
    
  })
}