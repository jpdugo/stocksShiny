box::use(
  dplyr[...],
  zoo,
  purrr,
  tibble,
  stringr,
  rvest,
  kableExtra,
  BatchGetSymbols,
  readr
)

#' Convert an xts object to a tibble
#'
#' This function converts an xts object to a tibble.
#'
#' @param xts_obj \code{xts}. Time series with named columns
#'
#' @return A tibble.
#' @export
#' @examples
#' # Load the xts package
#' library(xts)
#'
#' # Create a matrix with 3 columns and 5 rows
#' my_matrix <- matrix(
#'   c(
#'     10, 20, 30, 40, 50,
#'     15, 25, 35, 45, 55,
#'     12, 22, 32, 42, 52
#'   ),
#'   nrow = 5,
#'   ncol = 3,
#'   byrow = TRUE
#' )
#' # Create a vector of dates to use as row names
#' my_dates <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05"))
#'
#' # Create an xts object with the matrix and the dates
#' my_xts <- xts(my_matrix, order.by = my_dates)
#' colnames(my_xts) <- c("A", "B", "C")
#' xts_to_tibble(my_xts)
#'
#' @keywords xts tibble
xts_to_tibble <- function(xts_obj) {
  # todo something when xts has no column names
  data.frame(zoo$index(xts_obj), zoo$coredata(xts_obj)) |>
    purrr$set_names(c("date", names(xts_obj))) |>
    tibble$as_tibble()
}


#' Get table from finviz
#'
#' Download the table with indicators located below the graph in finviz
#'
#' @param ticker \code{character} a valid ticker name
#'
#' @return tibble
#' @export
#'
#' @examples
#' get_table_finviz("ATVI")
get_table_finviz <- function(ticker) {
  link <- stringr$str_c("https://finviz.com/quote.ashx?t=", ticker)
  url <- rvest$read_html(link)
  tbls <- url |> rvest$html_table()
  tbls[[9]]
}


#' A kable with no thead
#'
#' @param df a dataframe
#'
#' @return html table
#' @export
#'
#' @examples
#' no_thead_kable(mtcars)
no_thead_kable <- function(df) {
  x <- kableExtra$kable(
    x = df,
    format = "html",
    caption = "Finviz Table:"
  ) |>
    kableExtra$kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      font_size         = 12
    )

  gsub("<thead>.*</thead>", "", x)
}

box::use(
  quantmod,
  purrr
)

#' Get adjusted stock prices.
#'
#' Download the adjusted price of multiple tickers from yahoo and merge them
#' into one xts
#'
#' @param tickers \code[character] containing valid ticker names
#'
#' @return an xts object
#' @export
get_adjusted_prices <- function(tickers) {
  quantmod$getSymbols(tickers, env = environment())
  purrr$map(tickers, \(x) quantmod$Ad(get(x, envir = environment()))) |>
    purrr$reduce(merge)
}

#' @export
get_tickers_cleaned <- function() {
  BatchGetSymbols$GetSP500Stocks() |>
    mutate(
      Tickers = stringr$str_replace(Tickers, pattern = "\\.", "-")
    )
}


# get s&p tickers -----------------------------------------------------------------------------

#' @export
create_ticker_choices <- function() {
  get_tickers_cleaned() |>
    select(Tickers, Company) |>
    (\(x) {
      purrr$set_names(pull(x, Tickers), pull(x, Company))
    })()
}

#' @export
save_ticker_choices <- function() {
  create_ticker_choices() |> readr$write_rds("app/logic/sp500nms.rds")
}
