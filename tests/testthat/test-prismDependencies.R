test_that("prism Dependencies return html objects", {
  expect_s3_class(prismDependencies,class = 'shiny.tag')
  expect_s3_class(prismLanguageDependencies('r')[[1]],class = 'shiny.tag')
})
