# CI value for different hypo test

ci_median <- function(){
  DescTools
  }


ci_mean <- function(x){
  if(!is.vector(x)){
    print('only one dim vector surported!')
  }
  DescTools::MeanCI(x)
}

