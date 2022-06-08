

#' Title
#'
#' @param data a tibble
#' @param vars one or more data-mask var
#'
#' @return tibble
#' @export
#'
#' @examples
#' mtcars %>% tibble::as_tibble() %>% convert_as_factor(cyl)
convert_as_factor <- function(data, vars = NULL){
  if(rlang::is_null(substitute(vars))){
    print('no vars inputed!')
    return(data)
  } else if (is.character(substitute(vars))){
    data <- data %>% dplyr::mutate(across(vars, as.factor))
  } else {
    vars = rlang::enquo(vars)
    data <- data %>% dplyr::mutate(across(!! vars, as.factor))
  }
  data
}



