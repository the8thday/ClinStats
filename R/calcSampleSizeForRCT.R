#' Title
#'
#' @param alpha the significance level
#' @param gamma the power that you want
#' @param piT the estimated incidences of the disease in the disease
#' @param piC the estimated incidences of the disease in the control
#' @param p if we estimate that 10% of the people are likely to be lost to follow-up, then p=0.1
#'
#' @return a string
#' @export
#'
#' @examples calcSampleSizeForRCT(alpha=0.05, gamma=0.80, piT=0.15, piC=0.2)
#' This tells us that the sample size required in each group is 1214 people,
#' so overall we need 1214*2=2428 people in the randomised control trial.
calcSampleSizeForRCT <- function(alpha,gamma,piT,piC,p=0)
{
  # p is the estimated of the likely fraction of losses to follow-up
  qalpha <- qnorm(p=1-(alpha/2))
  qgamma <- qnorm(p=gamma)
  pi0 <- (piT + piC)/2
  numerator <- 2 * ((qalpha + qgamma)^2) * pi0 * (1 - pi0)
  denominator <- (piT - piC)^2
  n <- numerator/denominator
  n <- ceiling(n) # round up to the nearest integer
  # adjust for likely losses to folow-up
  n <- n/(1-p)
  n <- ceiling(n) # round up to the nearest integer
  print(paste("Sample size for each trial group = ",n))
}


#' Title
#'
#' @param alpha the significance level
#' @param piT the estimated incidences of the disease in the disease
#' @param piC the estimated incidences of the disease in the control
#' @param n number for each group
#'
#' @return
#' @export
#'
#' @examples
#' calcPowerForRCT(alpha=0.05, piT=0.2, piC=0.3, n=250)
#' for total 500 peopel
calcPowerForRCT <- function(alpha,piT,piC,n)
{
  qalpha <- qnorm(p=1-(alpha/2))
  pi0 <- (piT + piC)/2
  denominator <- 2 * pi0 * (1 - pi0)
  fraction <- n/denominator
  qgamma <- (abs(piT - piC) * sqrt(fraction)) - qalpha
  gamma <- pnorm(qgamma)
  print(paste("Power for the randomised controlled trial = ",gamma))
}
