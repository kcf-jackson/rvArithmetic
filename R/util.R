clean_up <- function(str0) {
  remove_extra_whitespace <- . %>% {gsub("  ", " ", .)}
  while (str0 != remove_extra_whitespace(str0)) {
    str0 <- remove_extra_whitespace(str0)
  }
  str0 %>% stringr::str_trim()
}


normalise <- function(vec0) { vec0 / sum(vec0) }


l2_norm <- function(x, y) { sum((x - y)^2) }


metric <- function(x, y, summary_fun, distance_fun = l2_norm) {
  summary_fun %>% purrr::map_dbl(~distance_fun(.x(x), .x(y))) %>% sum()
}
