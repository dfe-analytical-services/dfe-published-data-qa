context("screenFiles")

test_that("noFiltersFinalStage", {
  dataName <- "noFilters.csv"
  metaName <- "noFilters.meta.csv"

  data <- readFile("../../tests/shinytest/test-data/noFilters.csv")
  meta <- readFile("../../tests/shinytest/test-data/noFilters.meta.csv")

  screeningOutput <- screenFiles(dataName, metaName, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)

  expect_equal(screeningOutput$progress_message, "Made it to the full screening checks and passed")
})

test_that("noFiltersPassEverything", {
  dataName <- "noFilters.csv"
  metaName <- "noFilters.meta.csv"

  data <- readFile("../../tests/shinytest/test-data/noFilters.csv")
  meta <- readFile("../../tests/shinytest/test-data/noFilters.meta.csv")

  screeningOutput <- screenFiles(dataName, metaName, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("PassEverything", {
  dataName <- "passes_everything.csv"
  metaName <- "passes_everything.meta.csv"

  data <- readFile("../../tests/shinytest/test-data/passes_everything.csv")
  meta <- readFile("../../tests/shinytest/test-data/passes_everything.meta.csv")

  screeningOutput <- screenFiles(dataName, metaName, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)

  expect_equal(screeningOutput$results %>% filter(result != "PASS") %>% nrow(), 0)
})

test_that("QuotedBlanks-overcompleted_cols", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "overcompleted_cols") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-old_la_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "old_la_code") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-region_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "region_code") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})
