test_that("concatenation works", {

  # Create a Region object
  library(cronus)
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))

  # Create a model
  object1 <- new("PersephoneQuasiBin",
                 region = region,
                 crop = "Corn",
                 data = progress_ne$Corn,
                 formula = "Stage ~ Time + agdd") # PersephoneModel

  # Create another model
  object2 <- new("PersephoneCumLink",
                 region = region,
                 crop = "Soybeans",
                 data = progress_ne$Soybeans,
                 formula = "Stage ~ Time + agdd + adayl") # PersephoneModel

  # Concatenate the models
  object <- c(object1, object2) # PersephoneModelList

  # Check classes
  expect_s3_class(object, "PersephoneModelList")
  expect_s4_class(object[[1]], "PersephoneQuasiBin")
  expect_s4_class(object[[2]], "PersephoneCumLink")

})
