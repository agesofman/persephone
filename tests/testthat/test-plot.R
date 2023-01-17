test_that("plotting works", {

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

  # Fit
  object <- fit(object)

  # Plot
  expect_no_error(plot(object, cumulative = TRUE, seasons = 2002,
                       save = TRUE, dir = tempdir()))

  # Evaluate
  object <- crossval(object, maxsam = 10, seed = 1)
  expect_no_error(plot_metrics(object, save = TRUE, dir = tempdir()))

})
