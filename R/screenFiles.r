# screenFiles ------------------------------------------------------------
# function used to screen the data files, built upon other progress functions

source("R/fileValidation.r")
source("R/preCheck1.r")
source("R/preCheck2.r")
source("R/mainTests.r")

screenFiles <- function(datafilename, metafilename, dataseparator, metaseparator, data_character, meta_character, datafile, metafile) {
  output <- list("results" = fileValidation(datafilename, metafilename, dataseparator, metaseparator, datafile, metafile))
  if (any(output$results[["result"]] == "FAIL")) {
    # Failed file validation ---------------------------------------------------------------------------------

    output$progress_stage <- list(
      "src" = "www/progress_1.png",
      "alt" = "Failed at file validation stage, fix it then screen again"
    )

    output$progress_message <- "Failed at file validation stage, fix it then screen again"
  } else {
    # Pre-check stage 1 ---------------------------------------------------------------------------------

    output$results <- rbind(output$results, preCheck1(datafile, metafile))

    if (any(output$results[["result"]] == "FAIL")) {
      # Failed pre-checks at stage 1 ---------------------------------------------------------------------------------

      output$progress_stage <- list(
        "src" = "www/progress_2.png",
        "alt" = "Failed at pre-screening stage 1, fix it then screen again"
      )

      output$progress_message <- "Failed at pre-screening stage 1, fix it then screen again"
    } else {
      # Pre-check stage 2 ---------------------------------------------------------------------------------

      output$results <- rbind(output$results, preCheck2(data_character, meta_character, datafile, metafile))

      if (any(output$results[["result"]] == "FAIL")) {
        # Failed pre-checks stage 2 ---------------------------------------------------------------------------------

        output$progress_stage <- list(
          "src" = "www/progress_3.png",
          "alt" = "Failed at pre-screening stage 2, fix it then screen again"
        )

        output$progress_message <- "Failed at pre-screening stage 2, fix it then screen again"
      } else {
        # Main tests ----------------------------------------------------------------------------------

        output$results <- rbind(output$results, mainTests(data_character, meta_character, datafile, metafile))

        if (any(output$results[["result"]] == "FAIL")) {
          # Failed the main checks ---------------------------------------------------------------------------------

          output$progress_stage <- list(
            "src" = "www/progress_4.png",
            "alt" = "Failed at full checks stage, fix it then screen again"
          )

          output$progress_message <- "Made it to the full screening checks but failed"
        } else {
          # Passed all checks ---------------------------------------------------------------------------------

          output$progress_stage <- list(
            "src" = "www/progress_5.png",
            "alt" = "Made it to the full screening checks and passed"
          )

          output$progress_message <- "Made it to the full screening checks and passed"
        }
      }
    }
  }

  return(output)
}
