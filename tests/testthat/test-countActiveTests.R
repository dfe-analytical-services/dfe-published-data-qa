context("countActiveTests")

inputData <- list("datapath" = "../../tests/testthat/test-data/passes_everything.csv", "name" = "passes_everything.csv")
inputMeta <- list("datapath" = "../../tests/testthat/test-data/passes_everything.meta.csv", "name" = "passes_everything.meta.csv")
data <- readFile(inputData$datapath)
meta <- readFile(inputMeta$datapath)
passingFileResults <- screenFiles(inputData$name, inputMeta$name, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)$results

test_that("counting active tests", {
  expect_equal(
    numberActiveTests,
    nrow(passingFileResults)
  )
})
