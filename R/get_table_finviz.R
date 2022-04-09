get_table_finviz <- function(ticker) {
  link <- str_c("https://finviz.com/quote.ashx?t=", ticker)
  url <- read_html(link)
  tbls <- url %>% html_table()
  tbls[[9]]
  
  # x <-   
  # map_dfc(seq(1, 12, 2), ~ tbl[, c(.x, .x + 1)] %>%
  #     set_names(c("a", "b")) %>%
  #     pivot_wider(names_from = "a", values_from = "b", values_fn = list))
  # 
  # tbl <- x
  # walk(names(x), ~ {
  #   tbl <<- tbl %>% unnest_wider(sym(.x), names_sep = "_")
  # })
  # tbl %>% mutate(ticker = str_match(link, "=(.*)$")[[2]], .before = "Index")
}
