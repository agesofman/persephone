test_that("data are appropriate", {

  # Check class
  expect_s3_class(data_progress, "ProgressList")
  expect_s3_class(data_progress[[1]], "Progress")
  expect_s3_class(data_progress[[2]], "Progress")

})
