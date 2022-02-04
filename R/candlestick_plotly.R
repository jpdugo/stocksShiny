candlestick_plotly <- function(ochl_df, id) {
  fig <- plot_ly(
    data = ochl_df,
    x = ~date, type = "candlestick",
    open = as.formula(str_c("~", id, ".Open")), close = as.formula(str_c("~", id, ".Close")),
    high = as.formula(str_c("~", id, ".High")), low = as.formula(str_c("~", id, ".Low"))
  )
  fig %>% layout(title = id)
}