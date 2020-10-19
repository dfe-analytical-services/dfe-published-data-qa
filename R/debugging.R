# Debugging script
# Use this script to manually read in and screen files for those occassions when the app breaks without a useul error
#
# source("global.r")
#
# debugReadFile <- function(file) {
#   output <- readFile(file)
#   output$filename <- basename(file)
#   return(output)
# }
#
# # Put the file paths of the problematic files in here
# datafile <- debugReadFile("C:/R_projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.csv")
# metafile <- debugReadFile("C:/R_projects/dfe-published-data-qa/tests/shinytest/test-data/passes_everything.meta.csv")
#
# results <- screenFiles(datafile$filename, metafile$filename, datafile$fileSeparator, metafile$fileSeparator, datafile$fileCharacter, metafile$fileCharacter, datafile$mainFile, metafile$mainFile)
#
#
# data <- datafile$mainFile
# meta <- metafile$mainFile
