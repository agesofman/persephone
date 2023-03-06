test_that("prediction works", {

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

  # Fit
  object <- fit(object)

  # Predict
  data_predicted <- predict(object, data_progress)

  # Check class
  expect_s3_class(data_predicted, "ProgressList")
  expect_s3_class(data_predicted[[1]], "Progress")
  expect_s3_class(data_predicted[[2]], "Progress")

})
