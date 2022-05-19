#' calculate median CI for single vector
#'
#' @param x numberic vector
#' @inheritParams DescTools::MedianCI
#'
#' @return a tibble
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' ci_median(c(1,2,3,4,5))
ci_median <- function(x, conf.level = 0.95){
  if(!is.vector(x)){
    stop('only one dim vector surported!')
  }

  if(!'DescTools' %in% .packages()){
    loadNamespace('DescTools')
  }

  value <- DescTools::MedianCI(x,
                               conf.level=conf.level)
  as.data.frame(value) %>% t() %>%
    tibble::as_tibble()
  }



#' calculate mean CI for single vector
#'
#' @param x numeric vector
#'
#' @return mean CI tibble, same as t.test
#' @export
#'
#' @examples
#' ci_mean(mtcars$mpg)
ci_mean <- function(x, conf.level = 0.95){
  if(!is.numeric(x)){
    stop('only one dim vector surported!')
  }
  value <- DescTools::MeanCI(x,
                             conf.level = conf.level)
  as.data.frame(value) %>% t() %>%
    tibble::as_tibble()
}



#' calculate ci for proportion
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
ci_proportion <-  function(x, n=NULL, conf.level = 0.95){
  if(is.null(n)){
    DescTools::MultinomCI(x, conf.level = conf.level)
  } else{
    DescTools::BinomCI(x, n, conf.level = conf.level)
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



#' Confidence interval for a difference in means
#'
#' @param x numbers in sample1
#' @param y numbers in sample2
#' @param var.equal TRUE or FALSE
#' @param conf.level confidence level of the returned confidence interval
#' @param methods 't.test', 'pairwiseCI'
#'
#' @return a matrix
#' @export
#'
#' @examples
#' ci_diff_mean(x = c(1,2,3), y = c(3,4,5), methods = 't.test')
ci_diff_mean <- function(x, y, var.equal=FALSE,conf.level = 0.95,
                         methods = c('t.test', 'pairwiseCI')
                         ){
  if(methods == 't.test'){
    res <- t.test(x, y,
                  alternative = 'two.sided',
                  conf.level = conf.level,
                  var.equal=var.equal
    )
    return(res$conf.int)
  } else {
    res <- pairwiseCI::pairwiseCI(method = "Param.diff",
                                  var.equal=var.equal
                                  )
    return(res)
  }

}



#' Confidence interval for a difference in median
#'
#' @param formula A formula of the structure response ~ treatment for numerical variables
#' @param data A data.frame containing the numerical response variable and the treatment and by variable as factors
#' @param alternative Character string, either "two.sided", "less" or "greater"
#' @param conf.level The comparisonwise confidence level of the intervals
#'
#' @return a matrix
#' @export
#'
#' @examples
#' df <- data.frame(group = rep(c('a', 'b'),5), value = sample(1:10, 10))
#' ci_diff_median(value ~ group, data = df)
ci_diff_median <- function(formula, data,
                           alternative = "two.sided",
                           conf.level = 0.95
                           ){
  vv <- pairwiseCI::pairwiseCI(formula = formula,
                         data = data,
                         method = 'Median.diff',
                         alternative = alternative,
                         conf.level = conf.level
                         )
  vv
}



#' Confidence interval for a ratio in proportion
#'
#' @param formula A formula of the structure response ~ treatment for numerical variables
#' @param data A data.frame containing the numerical response variable and the treatment and by variable as factors
#' @param alternative Character string, either "two.sided", "less" or "greater"
#' @param conf.level The comparisonwise confidence level of the intervals
#'
#' @return
#' @export
#'
#' @examples
#' df <- data.frame(group = rep(c('a', 'b'),5), success = sample(1:10, 10), failure = sample(1:10, 10))
#' ci_proportion_ratio(cbind(success, failure) ~ group,data = df)
ci_proportion_ratio <- function(formula, data,
                                alternative = "two.sided",
                                conf.level = 0.95
                                ){
  pairwiseCI::pairwiseCI(formula = formula,
                         data = data,
                         method = 'Prop.ratio',
                         alternative = alternative,
                         conf.level = conf.level
                         )}













