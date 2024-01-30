context("readFile")

test_that("file separator", {
  expect_equal(readFile("../../tests/testthat/test-data/passes_everything.csv")$fileSeparator, ",")
})
