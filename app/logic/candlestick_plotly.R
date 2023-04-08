#' Create a candlestick chart with Plotly
#' 
#' Create a candlestick plot using a df with the same structure as the xts object returned by \code{quantmod::getSymbols()}
#'
#' @param ochl_df A data.frame containing open, close, high and low columns
#' @param id 'The name of the ticker included in the column names
#'
#' @return A Plotly object
#' @export
#'
candlestick_plotly <- function(ochl_df, id) {
  fig <- plotly::plot_ly(
    data = ochl_df,
    x = ~date, type = "candlestick",
    open = as.formula(stringr::str_c("~", id, ".Open")),
    close = as.formula(stringr::str_c("~", id, ".Close")),
    high = as.formula(stringr::str_c("~", id, ".High")),
    low = as.formula(stringr::str_c("~", id, ".Low"))
  )
  fig %>% plotly::layout(title = id) 
}