test_that("reporting works", {

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

  # Report
  expect_no_error(report(object, name = "example_report",
                         path = tempdir(), maxsam = 10))

})
