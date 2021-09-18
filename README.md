
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ClinStats

<!-- badges: start -->
<!-- badges: end -->

The goal of ClinStats is to calculate some clinical index in a very easy
and robust way,such as OR value, RR value, sample size for a RCT and on…

## Installation

You can install the released version of ClinStats from
[GitHub](https://github.com/the8thday/ClinStats) with:

``` r
remotes::install_github("the8thday/ClinStats")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ClinStats)

mymatrix <- matrix(c(156, 9421, 1531, 14797), nrow = 2, byrow = TRUE)
calcRelativeRisk(mymatrix)
#> [1] "category =  , relative risk =  0.173721236521721"
#> [1] "category =  ,  95 % confidence interval = [ 0.147624440337197 , 0.204431379720742 ]"
calcOddsRatio(mymatrix, alpha = 0.05, referencerow = 2 )
#> [1] "category =  , odds ratio =  0.160039091621751"
#> [1] "category =  ,  95 % confidence interval = [ 0.135460641900536 , 0.189077140693912 ]"
```

Note that: Some script come a little
[book](https://a-little-book-of-r-for-biomedical-statistics.readthedocs.io/en/latest/)
