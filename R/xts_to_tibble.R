xts_to_tibble <- function(xts_obj) {
  data.frame(index(xts_obj), coredata(xts_obj)) %>%
    set_names(c('date', names(xts_obj))) %>%
    as_tibble()
}