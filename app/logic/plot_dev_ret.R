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
  nm <- names(xts_ret) %>% stringr::str_extract("^.*\\.")
  df <- purrr::set_names(df, c("date", "Adjusted")) %>% na.omit()
  mn <- mean(xts_ret, na.rm = TRUE)
  
  #legends
  colors <- c("higher than 1stdev" = "green", "between 1stdev" = "blue", "lower than 1stdev" = "red")

  df %>%
    mutate(
      green = ifelse(Adjusted > as.numeric(mn) + as.numeric(stdev), Adjusted, NA),
      red = ifelse(Adjusted < as.numeric(mn) - as.numeric(stdev), Adjusted, NA),
      blue = ifelse(Adjusted < as.numeric(mn) + as.numeric(stdev) & Adjusted > mn - as.numeric(stdev), Adjusted, NA)
    ) %>%
    ggplot(aes(x = date)) +
    geom_point(aes(y = green, color = "higher than 1stdev")) +
    geom_point(aes(y = blue, color = "between 1stdev")) +
    geom_point(aes(y = red, color = "lower than 1stdev")) +
    labs(
      x = "Year",
      y = "Return",
      color = "Legend"
    ) +
    scale_color_manual(values = colors) +
    ggtitle(stringr::str_c(nm, 'returns colured by distance from the mean.')) +
    geom_hline(yintercept = mn)
}
