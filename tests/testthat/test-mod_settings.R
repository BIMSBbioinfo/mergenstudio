test_that("we can fetch available contexts", {
  expect_type(get_available_context(),"character")
})



test_that("we can fetch available services", {
  expect_type(get_api_services(),"character")
})
