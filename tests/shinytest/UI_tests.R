# Numbers correlate to expected/current json files

app <- ShinyDriver$new("../../")
app$snapshotInit("UI_tests", screenshot = FALSE)

# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
Sys.sleep(1)
app$snapshot()

# 2 Passes everything -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything.csv")
app$uploadFile(metafile = "test-data/passes_everything.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 3 - Passes everything advisory -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything_advisory.csv")
app$uploadFile(metafile = "test-data/passes_everything_advisory.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 4 - Passes everything ancillary  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything_ancillary.csv")
app$uploadFile(metafile = "test-data/passes_everything_ancillary.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 5 - Fail at prescreening stage 2  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_prescreening2.csv")
app$uploadFile(metafile = "test-data/fails_prescreening2.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 6 - Fail at pre-screening stage 1  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_prescreening1.csv")
app$uploadFile(metafile = "test-data/fails_prescreening1.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 7 - Fail at final stage  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_allchecks.csv")
app$uploadFile(metafile = "test-data/fails_allchecks.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 8 - Fail initial file validation  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_file validation.csv")
app$uploadFile(metafile = "test-data/fails_file validation.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 9 - Fail at final stage and ancillary  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_everything_ancillary.csv")
app$uploadFile(metafile = "test-data/fails_everything_ancillary.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 10 - Reset button -------------------------------------------------------------------------------------------------------------------
app$snapshot(items = list(
  input = c("resetbutton", "screenbutton", "shinyjs-resettable-datafile", "shinyjs-resettable-metafile"),
  output = c("advisory_box", "all_tests", "ancillary_box", "failed_box", "passed_box", "data_cols", "data_rows", "data_size", "datafilename", "file_exists", "meta_rows", "meta_cols", "meta_size", "metafilename", "num_advisory_tests", "num_failed_tests", "progress_stage", "showresults", "sum_combined_tests", "sum_failed_tests", "sum_ignored_tests", "sum_passed_tests", "summary_text", "table_advisory_tests", "table_all_tests", "table_failed_tests", "testtime"),
  export = TRUE
))
