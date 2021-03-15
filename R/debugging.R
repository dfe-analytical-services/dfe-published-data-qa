# Debugging script
# Use this script to manually read in and screen files for those occasions when the app breaks without a useful error

source("global.R")

debugReadFile <- function(file) {
  output <- readFile(file)
  output$filename <- basename(file)
  return(output)
}

datafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.csv")
metafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.meta.csv")

# Put the file paths of the files in here
datafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/mainTests/indicator_dp_completed.csv")
metafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/mainTests/indicator_dp_completed.meta.csv")

results <- screenFiles(datafile$filename, metafile$filename, datafile$fileSeparator, metafile$fileSeparator, datafile$fileCharacter, metafile$fileCharacter, datafile$mainFile, metafile$mainFile)

unlist(results$results$message)

data <- datafile$mainFile
meta <- metafile$mainFile

data <- datafile$fileCharacter
meta <- metafile$fileCharacter
