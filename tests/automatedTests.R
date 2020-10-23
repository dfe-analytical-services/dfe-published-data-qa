# Script to run both testthat and shinytest tests from command line

library(testthat)
library(shinytest)

cwd <- getwd()
setwd("../")
source("global.R")
testthat::test_dir("tests/testthat")
setwd(cwd)

test_that("UI tests", {
  expect_pass(shinytest::testApp("../", compareImages=FALSE, interactive = FALSE))
})
