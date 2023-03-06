test_that("tool functions works", {

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

  # Tools
  expect_type(get_crops(object), "character")
  expect_type(get_region(object), "character")
  expect_type(get_index(object, crop = "Corn"), "integer")
  expect_type(get_stages(object), "list")
  expect_type(get_seasons(object), "list")

})
