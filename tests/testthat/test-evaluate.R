test_that("evaluate works", {

  # Create a Region object
  library(cronus)
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))

  # Create a model
  object1 <- new("ProgressBM",
                 region = region,
                 crop = "Corn",
                 data = data_progress$Corn,
                 formula = "CumPercentage ~ Time + agdd") # ProgressModel

  # Create another model
  object2 <- new("ProgressCLM",
                 region = region,
                 crop = "Soybeans",
                 data = data_progress$Soybeans,
                 formula = "Stage ~ Time + agdd + adayl") # ProgressModel

  # Concatenate the models
  object <- c(object1, object2) # ProgressModelList

  # Evaluate
  expect_no_error(object <- evaluate(object, maxsam = 10, seed = 1))

  # Check classes
  expect_s3_class(object, "ProgressModelList")
  expect_s4_class(object[[1]], "ProgressBM")
  expect_s4_class(object[[2]], "ProgressCLM")

})
