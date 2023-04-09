# styler: off
box::use(
  dplyr[...],
  plotly,
  stringr,
  purrr,
  ggplot2,
  stats[as.formula, na.omit],
  app/logic/tables[xts_to_tibble]
)
# styler: on

#' Create a candlestick chart with Plotly
#'
#' Create a candlestick plot using a df with the same structure as the xts object returned by
#' \code{quantmod::getSymbols()}
#'
#' @param ochl_df A data.frame containing open, close, high and low columns
#' @param id 'The name of the ticker included in the column names
#'
#' @return A Plotly object
#' @export
#'
candlestick_plotly <- function(ochl_df, id) {
  fig <- plotly$plot_ly(
    data  = ochl_df,
    x     = ~date,
    type  = "candlestick",
    open  = as.formula(stringr$str_c("~", id, ".Open")),
    close = as.formula(stringr$str_c("~", id, ".Close")),
    high  = as.formula(stringr$str_c("~", id, ".High")),
    low   = as.formula(stringr$str_c("~", id, ".Low"))
  )
  fig |> plotly$layout(title = id)
}


#' Scatterplot with deviations from the mean
#'
#' Make a plot with the returns marked as green is they are above one standart
#' deviation and red is vice-versa.
#'
#' @param xts_ret xts object from \code{quantmod::getSymbols()}
#' @param stdev a standart deviation
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return ggplot
#' @export
#'
#'
plot_dev_ret <- function(xts_ret, stdev) {
  df <- xts_to_tibble(xts_ret)
  nm <- names(xts_ret) |> stringr$str_extract("^.*\\.")
  df <- purrr$set_names(df, c("date", "Adjusted")) |> na.omit()
  mn <- mean(xts_ret, na.rm = TRUE)

  # legends
  colors <- c(
    "higher than 1stdev" = "green",
    "between 1stdev"     = "blue",
    "lower than 1stdev"  = "red"
  )

  df |>
    mutate(
      green = ifelse(Adjusted > as.numeric(mn) + as.numeric(stdev), Adjusted, NA),
      red = ifelse(Adjusted < as.numeric(mn) - as.numeric(stdev), Adjusted, NA),
      blue = ifelse(
        Adjusted < as.numeric(mn) + as.numeric(stdev) & Adjusted > mn - as.numeric(stdev),
        Adjusted,
        NA
      )
    ) |>
    ggplot2$ggplot(ggplot2$aes(x = date)) +
    ggplot2$geom_point(ggplot2$aes(y = green, color = "higher than 1stdev")) +
    ggplot2$geom_point(ggplot2$aes(y = blue, color = "between 1stdev")) +
    ggplot2$geom_point(ggplot2$aes(y = red, color = "lower than 1stdev")) +
    ggplot2$labs(
      x = "Year",
      y = "Return",
      color = "Legend"
    ) +
    ggplot2$scale_color_manual(values = colors) +
    ggplot2$ggtitle(stringr::str_c(nm, "returns colured by distance from the mean.")) +
    ggplot2$geom_hline(yintercept = mn)
}
