testIndividualTest <- function(pathStart, check) {
  dataFilePath <- paste0(pathStart, check, ".csv")
  metaFilePath <- paste0(pathStart, check, ".meta.csv")
  inputData <- list("datapath" = dataFilePath)
  inputData$name <- basename(inputData$datapath)
  inputMeta <- list("datapath" = metaFilePath)
  inputMeta$name <- basename(inputMeta$datapath)
  data <- readFile(inputData$datapath)
  meta <- readFile(inputMeta$datapath)
  return(screenFiles(inputData$name, inputMeta$name, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile)$results %>% filter(test == check) %>% pull(result) %>% unlist())
}

testIndividualTestSeparate <- function(dataFilePath, metaFilePath, check) {
  inputData <- list("datapath" = dataFilePath)
  inputData$name <- basename(inputData$datapath)
  inputMeta <- list("datapath" = metaFilePath)
  inputMeta$name <- basename(inputMeta$datapath)
  data <- readFile(inputData$datapath)
  meta <- readFile(inputMeta$datapath)
  results <- screenFiles(
    inputData$name,
    inputMeta$name,
    data$fileSeparator,
    meta$fileSeparator,
    data$fileCharacter,
    meta$fileCharacter,
    data$mainFile,
    meta$mainFile
  )$results
  if (check %in% results$test) {
    return(
      results %>%
        filter(test == check) %>%
        pull(result) %>%
        unlist()
    )
  } else {
    warning_message <- paste(check, "not found in results")
    warning(warning_message)
    print(results$test |> unlist())
    return(warning_message)
  }
}

testOther <- function(dataFilePath) {
  metaFilePath <- sub("\\.csv$", "\\.meta.csv", dataFilePath)

  dataName <- basename(dataFilePath)
  metaName <- basename(metaFilePath)

  data <- readFile(dataFilePath)
  meta <- readFile(metaFilePath)

  return(screenFiles(dataName, metaName, data$fileSeparator, meta$fileSeparator, data$fileCharacter, meta$fileCharacter, data$mainFile, meta$mainFile))
}
