# Debugging script
# Use this script to manually read in and screen files for those occasions when the app breaks without a useful error
# Helpful for running code in console
# Uncomment when you need to use this, and always make sure to comment it again before committing

# # Dependencies ==============================================================
# source("global.R")
# source("R/readFile.R")
# source("R/screenFiles.R")
# source("R/knownVariables.R")
# source("R/fileValidation.R")
# source("R/preCheck1.R")
# source("R/preCheck2.R")
# source("R/mainTests.R")
#
#
# debugReadFile <- function(file) {
#   output <- readFile(file)
#   output$filename <- basename(file)
#   return(output)
# }
#
# # Choose files ==============================================================
# datafile <- debugReadFile(file.choose())
# metafile <- debugReadFile(file.choose())
#
# # Screen files ==============================================================
# results <- screenFiles(datafile$filename, metafile$filename, datafile$fileSeparator, metafile$fileSeparator, datafile$fileCharacter, metafile$fileCharacter, datafile$mainFile, metafile$mainFile)
#
# data.table(results$results) %>% View()
#
# # Extra helpers =============================================================
# data <- datafile$mainFile
# meta <- metafile$mainFile
#
# data <- datafile$fileCharacter
# meta <- metafile$fileCharacter
