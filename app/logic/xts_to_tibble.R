box::use(
  zoo,
  purrr,
  tibble
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