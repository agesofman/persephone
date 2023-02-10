test_that("prediction works", {

  # Create a Region object
  library(cronus)
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))

  # Create a model
  object1 <- new("PersephoneBin",
                 region = region,
                 crop = "Corn",
                 data = progress_ne$Corn,
                 formula = "CumPercentage ~ Time + agdd") # PersephoneModel

  # Create another model
  object2 <- new("PersephoneCumLink",
                 region = region,
                 crop = "Soybeans",
                 data = progress_ne$Soybeans,
                 formula = "Stage ~ Time + agdd + adayl") # PersephoneModel

  # Concatenate the models
  object <- c(object1, object2) # PersephoneModelList

  # Fit
  object <- fit(object)

  # Predict
  data_predicted <- predict(object, progress_ne)

  # Tests
  expect_type(data_predicted, "list")
  expect_type(data_predicted[[1]], "list")
  expect_type(data_predicted[[2]], "list")

})