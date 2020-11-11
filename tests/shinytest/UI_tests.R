# Numbers correlate to expected/current json files

app <- ShinyDriver$new("../../")
app$snapshotInit("UI_tests", screenshot = FALSE)

# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
Sys.sleep(1)
app$snapshot()

# 2 Passes everything -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 3 - Passes everything advisory -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything_advisory.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything_advisory.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 4 - Passes everything ancillary  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 5 - Fail at prescreening stage 2  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_prescreening2.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_prescreening2.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 6 - Fail at pre-screening stage 1  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_prescreening1.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_prescreening1.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 7 - Fail at final stage  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_allchecks.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_allchecks.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 8 - Fail initial file validation  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_file validation.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_file validation.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 9 - Fail at final stage and ancillary  -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/fails_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 10 - Reset button -------------------------------------------------------------------------------------------------------------------
app$snapshot(items = list(
  input = c("resetbutton","screenbutton","shinyjs-resettable-datafile","shinyjs-resettable-metafile"),
  output = c("advisory_box","advisory_box","all_tests","ancillary_box","data_cols","data_rows","data_size","datafilename","failed_box","file_exists",
             "meta_cols","meta_rows","meta_size","metafilename","num_advisory_tests","num_failed_tests","passed_box","progress_stage","showresults","sum_combined_tests",
             "sum_failed_tests","sum_ignored_tests","sum_passed_tests","summary_text","table_advisory_tests","table_all_tests","table_failed_tests","testtime",
             "uiResetButton","uiScreenButton")
))             
             
             
             

