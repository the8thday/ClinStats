# CI value for different hypo test

ci_median <- function(x){
  if(!is.vector(x)){
    stop('only one dim vector surported!')
  }
  value <- DescTools::MeanCI(x)
  as.data.frame(value) %>% t() %>%
    tibble::as_tibble()
  }


ci_mean <- function(x){
  if(!is.vector(x)){
    stop('only one dim vector surported!')
  }
  value <- DescTools::MeanCI(x)
  as.data.frame(value) %>% t() %>%
    tibble::as_tibble()
}


ci_proportion <-  function(x, n=NULL){
  if(is.null(n)){
    DescTools::MultinomCI(x)
  } else{
    DescTools::BinomCI(x, n)
  }
}


ci_diff_proportion <- function(){}


