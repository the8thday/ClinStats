# CI value for different hypo test

#' Title
#'
#' @param x
#'
#' @return a tibble
#' @export
#'
#' @examples
#' ci_median(c(3,4,5,6,7,8))
ci_median <- function(x){
  if(!is.vector(x)){
    stop('only one dim vector surported!')
  }

  if(!'DescTools' %in% .packages()){
    loadNamespace('DescTools')
  }

  value <- DescTools::MedianCI(x)
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


ci_diff_proportion <- function(x1, n1, x2, n2, conf.level = 0.95,
                               sides = c("two.sided","left","right"),
                               method = c("ac", "wald", "waldcc", "score", "scorecc", "mn",
                                          "mee", "blj", "ha", "hal", "jp")){
  stopifnot(is.integer(x1)&is.integer()){}
  method <- match.arg()

  DescTools::BinomDiffCI(x1,n1,x2,n2)
}

ci_diff_mean <- function(){}


ci_diff_median <- function(){
  pairwiseCI::pairwiseCI(method = 'Median.diff')
}


ci_ratio_proportion <- function(){
  pairwiseCI::pairwiseCI(method = 'Prop.ratio')
}


ci_ratio_mean <- function(){}


ci_ratio_median <- function(){}











