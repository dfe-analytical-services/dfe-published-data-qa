source("renv/activate.R")

tidy_code <- function() {
  source("global.r")
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  styler::style_dir(recursive = FALSE)
  message("R scripts")
  message("----------------------------------------")
  styler::style_dir("R/")
  message("Test scripts")
  message("----------------------------------------")
  styler::style_dir("tests/", filetype = "r")
}

run_tests_locally <- function() {
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  source("global.r")
  message("================================================================================")
  message("== testthat ====================================================================")
  message("")
  testthat::test_dir("tests/testthat")
  message("")
  message("================================================================================")
  message("== shinytest ===================================================================")
  message("")
  shinytest::testApp()
  message("")
  message("================================================================================")
}
