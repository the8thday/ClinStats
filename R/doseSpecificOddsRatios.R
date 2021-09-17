doseSpecificOddsRatios <- function(mymatrix,referencerow=1)
{
  numstrata <- nrow(mymatrix)
  # calculate the stratum-specific odds ratios, and odds of disease:
  doses <- as.numeric(rownames(mymatrix))
  for (i in 1:numstrata)
  {
    dose <- doses[i]
    # calculate the odds ratio:
    DiseaseExposed <- mymatrix[i,1]
    DiseaseUnexposed <- mymatrix[i,2]
    ControlExposed <- mymatrix[referencerow,1]
    ControlUnexposed <- mymatrix[referencerow,2]
    totExposed <- DiseaseExposed + ControlExposed
    totUnexposed <- DiseaseUnexposed + ControlUnexposed
    probDiseaseGivenExposed <- DiseaseExposed/totExposed
    probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
    probControlGivenExposed <- ControlExposed/totExposed
    probControlGivenUnexposed <- ControlUnexposed/totUnexposed
    oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
      (probControlGivenExposed*probDiseaseGivenUnexposed)
    print(paste("dose =", dose, ", odds ratio = ",oddsRatio))
  }
}
