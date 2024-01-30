options(renv.config.sandbox.enabled = FALSE)
source("renv/activate.R")

shhh <- suppressPackageStartupMessages # It's a library, so shhh!

tidy_code <- function() {
  shhh(source("global.r"))
  shhh(tidy_code_function())
}

run_tests_locally <- function() {
  shhh(source("global.r"))
  Sys.unsetenv("HTTP_PROXY")
  test_app()
}
