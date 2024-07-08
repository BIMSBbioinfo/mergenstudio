test_that(".onLoad sets options appropriately", {
  .onLoad()
  expect_false(getOption("mergenstudio.valid_api"))
  expect_null(getOption("mergenstudio.openai_key"))
})
