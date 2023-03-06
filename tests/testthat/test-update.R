test_that("update works", {

  # Create a Region object
  library(cronus)
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))

  # Create model
  object <- new("ProgressCLM", region = region, crop = "Corn", model = "A")

  # Update model
  expect_no_error(object <- update(object, region = region, crop = "Soybeans", model = "B"))

  # Check class
  expect_s4_class(object, "ProgressCLM")

})
