#' Get adjusted stock prices.
#' 
#' Download the adjusted price of multiple tickers from yahoo and merge them
#' into one xts
#'
#' @param tickers a vector of strings containing valid ticker names
#'
#' @return an xts object
#' @export
get_adjusted_prices <- function(tickers) {
  quantmod::getSymbols(tickers, env = environment())
  purrr::map(tickers, ~ quantmod::Ad(get(.x, envir = environment()))) %>%
    purrr:reduce(merge)
}
