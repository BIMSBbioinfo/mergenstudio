test_that("welcome message is an html widget", {
  expect_s3_class(welcomeMessage(),'htmlwidget')
})


test_that("default welcome message holds certain keys",{
  output <- chat_message_default()

  # Define the expected keys
  expected_keys <- c("role", "content")

  # Check if all expected keys are present in the output list
  expect_true(all(expected_keys %in% names(output)))

  #check that role is assistant
  expect_true(output$role == 'assistant')
})
