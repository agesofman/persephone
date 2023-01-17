test_that("data are appropriate", {

  # Check types
  expect_type(progress_ne, "list")
  expect_type(progress_ne[[1]], "list")
  expect_type(progress_ne[[2]], "list")

  # Check Variables
  vars <- c("Season", "Time", "Date", "Percentage", "CumPercentage")
  expect_true(min(vars %in% names(progress_ne[[1]])) == 1)
  expect_true(min(vars %in% names(progress_ne[[2]])) == 1)

})
