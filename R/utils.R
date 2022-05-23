
get_dot_vars <- function(...){
  rlang::quos(...) %>% purrr::map(rlang::quo_text) %>% unlist()
}
