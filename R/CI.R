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


ci_portation <-  function(x){
  DescTools::BinomCI()
}



