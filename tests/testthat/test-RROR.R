test_that("calculate RR or OR", {
  mymatrix <- matrix(c(156, 9421, 1531, 14797),
                     nrow = 2, byrow = TRUE
  )
  colnames(mymatrix) <- c("Disease", "Control")
  rownames(mymatrix) <- c("Exposed", "Unexposed")

  calcRelativeRisk(mymatrix)
})
