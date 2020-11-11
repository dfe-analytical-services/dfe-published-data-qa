# Debugging script
# Use this script to manually read in and screen files for those occasions when the app breaks without a useful error

source("global.r")

debugReadFile <- function(file) {
  output <- readFile(file)
  output$filename <- basename(file)
  return(output)
}

# Put the file paths of the files in here
datafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/preCheck2/time_validation.csv")
metafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/preCheck2/time_validation.meta.csv")

results <- screenFiles(datafile$filename, metafile$filename, datafile$fileSeparator, metafile$fileSeparator, datafile$fileCharacter, metafile$fileCharacter, datafile$mainFile, metafile$mainFile)

unlist(results$results$message)

data <- datafile$mainFile
meta <- metafile$mainFile

data <- datafile$fileCharacter
meta <- metafile$fileCharacter
