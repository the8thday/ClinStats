test_that("Calculate RR or OR", {
  mymatrix <- matrix(c(156, 9421, 1531, 14797),
                     nrow = 2, byrow = TRUE
  )
  colnames(mymatrix) <- c("Disease", "Control")
  rownames(mymatrix) <- c("Exposed", "Unexposed")

  expect_output(calcRelativeRisk(mymatrix),
                'category = Exposed , relative risk =  0.173721236521721')
})
