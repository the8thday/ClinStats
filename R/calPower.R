#' calcPowerForRCT
#'
#' calcPowerForRCT
#'
#' @param alpha the significance level
#' @param piT the estimated incidences of the disease in the disease
#' @param piC the estimated incidences of the disease in the control
#' @param n number for each group
#'
#' @return string
#' @export
#'
#' @examples
#' calcPowerForRCT(alpha=0.05, piT=0.2, piC=0.3, n=250)
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
