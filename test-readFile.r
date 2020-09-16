context("readFile")

test_that("file separator", {
  expect_equal(readFile("../../tests/shinytest/test-data/passes_everything.csv")$fileSeparator, ",")
})
