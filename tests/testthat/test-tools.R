test_that("tool functions works", {

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

  # Tools
  expect_type(get_crops(object), "character")
  expect_type(get_region(object), "character")
  expect_type(get_index(object, crop = "Corn"), "integer")
  expect_type(get_stages(object), "list")
  expect_type(get_seasons(object), "list")

})
