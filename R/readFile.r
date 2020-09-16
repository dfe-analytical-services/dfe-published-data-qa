# readFiles --------------------------------------------------

readFile <- function(filePath) {
  fileSeparator <- capture.output(mainFile <- fread(filePath, encoding = "UTF-8", na.strings = "", verbose = TRUE, strip.white = FALSE)) %>%
    .[grepl("sep=.* with", .)] %>%
    sub("with.*$", "", x = .) %>%
    trimws(.) %>%
    str_remove(., "^sep='") %>%
    str_remove(., "'$")

  output <- list(
    "mainFile" = mainFile,
    "fileSeparator" = fileSeparator,
    "fileCharacter" = fread(filePath, encoding = "UTF-8", colClasses = c("character"), na.strings = NULL)
  )

  return(output)
}
