app <- ShinyDriver$new("../../")
app$snapshotInit("resetButton")

app$uploadFile(datafile = "test-data/fails_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/fails_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
Sys.sleep(0.5)
app$setInputs(resetbutton = "click")
app$snapshot()
