calcTaronesTest <- function(mylist, referencerow = 2) {
  require("metafor")
  numstrata <- length(mylist)
  # make an array "ntrt" of the number of people in the exposed group, in each stratum
  # make an array "nctrl" of the number of people in the unexposed group, in each stratum
  # make an array "ptrt" of the number of people in the exposed group that have the disease,
  # in each stratum
  # make an array "pctrl" of the number of people in the unexposed group that have the disease,
  # in each stratum
  # make an array "htrt" of the number of people in the exposed group that don't have the
  # disease, in each stratum
  # make an array "hctrl" of the number of people in the unexposed group that don't have the
  # disease, in each stratum
  ntrt <- vector()
  nctrl <- vector()
  ptrt <- vector()
  pctrl <- vector()
  htrt <- vector()
  hctrl <- vector()
  if (referencerow == 1) {
    nonreferencerow <- 2
  } else {
    nonreferencerow <- 1
  }
  for (i in 1:numstrata)
  {
    mymatrix <- mylist[[i]]
    DiseaseUnexposed <- mymatrix[referencerow, 1]
    ControlUnexposed <- mymatrix[referencerow, 2]
    totUnexposed <- DiseaseUnexposed + ControlUnexposed
    nctrl[i] <- totUnexposed
    pctrl[i] <- DiseaseUnexposed
    hctrl[i] <- ControlUnexposed
    DiseaseExposed <- mymatrix[nonreferencerow, 1]
    ControlExposed <- mymatrix[nonreferencerow, 2]
    totExposed <- DiseaseExposed + ControlExposed
    ntrt[i] <- totExposed
    ptrt[i] <- DiseaseExposed
    htrt[i] <- ControlExposed
  }
  # calculate Tarone's test of homogeneity, using the rma.mh function from the
  # "metafor" package
  tarone <- rma.mh(ptrt, htrt, pctrl, hctrl, ntrt, nctrl)
  pvalue <- tarone$TAp
  print(paste("Pvalue for Tarone's test =", pvalue))
}
