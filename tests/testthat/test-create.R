test_that("create works", {

  # Create a Region object
  library(cronus)
  region <- Region(name = "nebraska", type = "us state",
                   div = c(country = "United States", state = "Nebraska"))

  # Class
  Class <- "ProgressBM"

  # Formula
  formula <- list(c(global = "CumPercentage ~ Time + agdd * adayl"),
                  c(base = "CumPercentage ~ Time + agdd"))

  # Link
  link <- list("logit", "probit")

  # Set the parameters
  prm <- cronus::combine(Class = Class,
                         formula = formula,
                         link = link,
                         region = list(region),
                         data = ProgressList(),
                         crop = "Corn")

  # Create the object
  object <- create(prm)

  # Test
  expect_s3_class(object, "ProgressModelList")
  expect_true(length(object) == 4)

})
