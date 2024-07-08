test_that("fetching models works", {
  expect_type(get_available_models('openai'),'character')
})



test_that("fetching models works only when service is provided", {
  expect_error(get_available_models())
})
