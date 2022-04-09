#' Get table from finviz
#' 
#' Download the table with indicators located below the graph in finviz
#'
#' @param ticker a valid ticker name 
#'
#' @return tibble
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#'get_table_finviz('ATVI')
get_table_finviz <- function(ticker) {
  link <- stringr::str_c("https://finviz.com/quote.ashx?t=", ticker)
  url <- rvest::read_html(link)
  tbls <- url %>% rvest::html_table()
  tbls[[9]]
}
