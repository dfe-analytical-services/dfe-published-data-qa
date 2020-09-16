app <- ShinyDriver$new("../../")
app$snapshotInit("screenButton", screenshot = FALSE)

# 1
app$uploadFile(datafile = "test-data/passes_everything.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 2
app$uploadFile(datafile = "test-data/passes_everything_advisory.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything_advisory.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 3
app$uploadFile(datafile = "test-data/passes_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 4
app$uploadFile(datafile = "test-data/fails_prescreening2.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_prescreening2.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 5
app$uploadFile(datafile = "test-data/fails_prescreening1.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_prescreening1.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 6
app$uploadFile(datafile = "test-data/fails_allchecks.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_allchecks.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 7
app$uploadFile(datafile = "test-data/fails_file validation.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_file validation.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))

app$setInputs(resetbutton = "click")

# 8
app$uploadFile(datafile = "test-data/fails_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests", "fail_box", "passed_box", "advisory_box", "ancillary_box")))
