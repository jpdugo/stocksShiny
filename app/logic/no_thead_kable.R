#' A kable with no thead
#'
#' @param df a dataframe
#'
#' @return html table
#' 
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#'no_thead_kable(mtcars)
no_thead_kable <- function(df) {
  x <- kableExtra::kable(df, format = "html", caption = "Finviz Table:") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12)
  gsub("<thead>.*</thead>", "", x)
}
