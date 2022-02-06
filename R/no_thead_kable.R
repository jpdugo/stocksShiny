no_thead_kable <- function(df) {
  x <- kable(df, format = "html", caption = "Finviz Table:") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12)
  gsub("<thead>.*</thead>", "", x)
}
