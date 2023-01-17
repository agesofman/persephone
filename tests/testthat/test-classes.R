test_that("class definition works", {

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

  expect_s4_class(object1, "PersephoneQuasiBin")

  # Create another model
  object2 <- new("PersephoneCumLink",
                 region = region,
                 crop = "Soybeans",
                 data = progress_ne$Soybeans,
                 formula = "Stage ~ Time + agdd + adayl") # PersephoneModel

  expect_s4_class(object2, "PersephoneCumLink")

})
