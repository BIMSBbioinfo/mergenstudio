test_that("icon labels are html objects", {
  expect_s3_class(getIconLabel(),class = 'shiny.tag')
})
