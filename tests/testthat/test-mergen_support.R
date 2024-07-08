test_that("evaluating an error returns a string", {
  err<-evaluate_error('nofunction()')
  expect_type(err,"character")
})



test_that("evaluating a normal expression returns null", {
  res<-evaluate_error('x<-5')
  expect_equal(res,NULL)

})
