test_that("validation of created objects during filereading works", {
  expect_equal(check_data('x<-data.table::fread()'),NULL)
})
