# Debugging script
# Use this script to manually read in and screen files for those occassions when the app breaks without a useul error

source("global.r")

debugReadFile <- function(file) {
  output <- readFile(file)
  output$filename <- basename(file)
  return(output)
}

# Put the file paths of the problematic files in here
datafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/mainTests/filter_group_stripped.csv")
metafile <- debugReadFile("C:/R_Projects/dfe-published-data-qa/tests/testthat/mainTests/filter_group_stripped.meta.csv")

results <- screenFiles(datafile$filename, metafile$filename, datafile$fileSeparator, metafile$fileSeparator, datafile$fileCharacter, metafile$fileCharacter, datafile$mainFile, metafile$mainFile)

unlist(results$results$message)

data <- datafile$mainFile
meta <- metafile$mainFile

data <- datafile$fileCharacter
meta <- metafile$fileCharacter
