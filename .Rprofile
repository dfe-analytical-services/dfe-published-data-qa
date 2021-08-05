source("renv/activate.R")

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

tidy_code <- function() {
  shhh(source("global.r"))
  shhh(tidy_code_function())
}

run_tests_locally <- function() {
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  shhh(source("global.r"))
  message("================================================================================")
  message("== testthat ====================================================================")
  message("")
  shhh(testthat::test_dir("tests/testthat"))
  message("")
  message("================================================================================")
  message("== shinytest ===================================================================")
  message("")
  shhh(shinytest::testApp())
  message("")
  message("================================================================================")
}
