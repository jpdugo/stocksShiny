#' Convert an xts to a tibble
#'
#' @param xts_obj xts object
#'
#' @return tibble
#' @export
#'
xts_to_tibble <- function(xts_obj) {
  data.frame(zoo::index(xts_obj), zoo::coredata(xts_obj)) %>%
    purrr::set_names(c('date', names(xts_obj))) %>%
    tibble::as_tibble()
}