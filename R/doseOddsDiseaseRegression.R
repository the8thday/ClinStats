#' Title calculate OR value by lR
#'
#' @param mymatrix a two dimension matrix
#' @param referencerow which disease row as reference
#'
#' @return string with results
#' @export
#'
#' @examples
doseOddsDiseaseRegression <- function(mymatrix, referencerow = 1) {
  numstrata <- nrow(mymatrix)
  # calculate the stratum-specific odds ratios, and odds of disease:
  myodds <- vector()
  doses <- as.numeric(rownames(mymatrix))
  for (i in 1:numstrata)
  {
    dose <- doses[i]
    # calculate the odds of disease given exposure:
    DiseaseExposed <- mymatrix[i, 1]
    ControlExposed <- mymatrix[i, 2]
    totExposed <- DiseaseExposed + ControlExposed
    probDiseaseGivenExposed <- DiseaseExposed / totExposed
    probNotDiseaseGivenExposed <- ControlExposed / totExposed
    odds <- probDiseaseGivenExposed / probNotDiseaseGivenExposed
    logodds <- log(odds) # this is the natural log
    myodds[i] <- logodds
  }

  # test whether the regression line of log(odds) versus has a zero slope or not:
  lm1 <- lm(myodds ~ doses)
  summarylm1 <- summary(lm1)
  coeff1 <- summarylm1$coefficients
  # get the p-value for the F-test that the slope is not zero:
  pvalue <- coeff1[2, 4]
  print(paste("pvalue for F-test of zero slope =", pvalue))

  # make a plot of log(odds) versus dose:
  plot(doses, myodds, xlab = "Dose", ylab = "log(odds)", main = "Plot of log(odds) versus dose")
}
