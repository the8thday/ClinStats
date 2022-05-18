# CI value for different hypo test

#' Title
#'
#' @param x
#'
#' @return a tibble
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
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


#' calculate mean CI for single vector
#'
#' @param x numeric vector
#'
#' @return mean CI tibble
#' @export
#'
#' @examples
#' ci_mean(mtcars$mpg)
ci_mean <- function(x){
  if(!is.numeric(x)){
    stop('only one dim vector surported!')
  }
  value <- DescTools::MeanCI(x)
  as.data.frame(value) %>% t() %>%
    tibble::as_tibble()
}


#' Title
#'
#' @param x vector of positive integers representing the number of occurrences of each class, or number of
#' successes(n is needed)
#' @param n number of trials.
#'
#' @return A vector with 3 elements for estimate, lower confidence intervall and upper for the upper one.

#' @export
#'
#' @examples
#' ci_proportion(c(2,3,4))
#' ci_proportion(x = 37, n = 43)
ci_proportion <-  function(x, n=NULL){
  if(is.null(n)){
    DescTools::MultinomCI(x)
  } else{
    DescTools::BinomCI(x, n)
  }
}


#' Title
#'
#' @param x1 number of success for the first group
#' @param n1 number of trails for the first group
#' @param x2 number of successes for the second group
#' @param n2 number of trails for the second group
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence interval,
#' must be one of "two.sided" (default), "left" or "right".
#' @param method one of "wald", "waldcc", "ac", "score", "scorecc", "mn", "mee", "blj", "ha", "hal", "jp".
#'
#' @return A matrix with 3 columns containing the estimate, the lower and the upper confidence intervall.
#' @export
#'
#' @examples
#' ci_diff_proportion(x1=56, n1=70, x2=48, n2=80)
ci_diff_proportion <- function(x1, n1, x2, n2, conf.level = 0.95,
                               sides = c("two.sided","left","right"),
                               method = c("ac", "wald", "waldcc", "score", "scorecc", "mn",
                                          "mee", "blj", "ha", "hal", "jp")){
  stopifnot(is.numeric(x1)&is.numeric(n1))
  if(missing(method)) method <- match.arg(method)
  if(missing(sides)) sides <- match.arg(sides)

  res <- DescTools::BinomDiffCI(x1,n1,x2,n2,
                         conf.level = conf.level,
                         sides = sides,
                         method = method
                         )
  res
}



ci_diff_mean <- function(x, y, var.equal=FALSE,
                         methods = c('t.test', 'pairwiseCI')
                         ){
  res <- t.test(x, y,
                alternative = 'two.sided',
                onf.level = 0.95,
                var.equal=var.equal
                )
  res$conf.int
}



ci_diff_median <- function(formula, data,
                           alternative = "two.sided",
                           conf.level = 0.95
                           ){
  pairwiseCI::pairwiseCI(method = 'Median.diff')
}


ci_proportion_ratio <- function(){
  pairwiseCI::pairwiseCI(method = 'Prop.ratio')
}


ci_mean_ratio <- function(){}


ci_media_ration <- function(){}











