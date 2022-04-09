get_adjusted_prices <- function(tickers) {
  getSymbols(tickers, env = environment())
  map(tickers, ~ Ad(get(.x, envir = environment()))) %>%
    reduce(merge)
}
