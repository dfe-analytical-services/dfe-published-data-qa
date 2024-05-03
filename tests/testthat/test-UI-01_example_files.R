library(shinytest2)

app <- AppDriver$new(name = "example_files", height = 846, width = 1445, load_timeout = 45 * 1000, timeout = 20 * 1000, wait = TRUE, expect_values_screenshot_args = FALSE)

# Wait until Shiny is not busy for 5ms
app$wait_for_idle(5)

# Single test_that function to save needing to restart the app each time
test_that("Passes everything", {
  # 1. Initial load (JSON 1)
  app$expect_values()

  # 2. Passes everything
  app$upload_file(datafile = "test-data/passes_everything.csv")
  app$upload_file(metafile = "test-data/passes_everything.meta.csv")
  app$set_inputs(screenbutton = "click")

  # Make sure it gets to the passed stage and no tests are ignored, advisory or failed
  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")
  expect_true(app$get_value(export = "failed") == 0)
  expect_true(app$get_value(export = "advisory") == 0)
  expect_true(app$get_value(export = "ignored") == 0)

  app$set_inputs(resetbutton = "click") # Reset app

  # 3. Passes everything advisory
  app$upload_file(datafile = "test-data/passes_everything_advisory.csv")
  app$upload_file(metafile = "test-data/passes_everything_advisory.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")
  expect_true(app$get_value(export = "failed") == 0)
  expect_true(app$get_value(export = "advisory") > 0)

  app$set_inputs(resetbutton = "click") # Reset app

  # 4. Passes everything ancillary
  app$upload_file(datafile = "test-data/passes_everything_ancillary.csv")
  app$upload_file(metafile = "test-data/passes_everything_ancillary.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")
  expect_true(app$get_value(export = "failed") == 0)
  expect_true(app$get_value(export = "ancillary") > 0)

  app$set_inputs(resetbutton = "click") # Reset app

  # 5. Fail initial file validation
  app$upload_file(datafile = "test-data/fails_file validation.csv")
  app$upload_file(metafile = "test-data/fails_file validation.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Failed at file validation stage, fix it then screen again")
  expect_true(app$get_value(export = "failed") == 2)

  app$set_inputs(resetbutton = "click") # Reset app

  # 6. Fail at prescreening stage 1
  app$upload_file(datafile = "test-data/fails_prescreening1.csv")
  app$upload_file(metafile = "test-data/fails_prescreening1.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Failed at pre-screening stage 1, fix it then screen again")

  app$set_inputs(resetbutton = "click") # Reset app

  # 6. Fail at prescreening stage 2
  app$upload_file(datafile = "test-data/fails_prescreening2.csv")
  app$upload_file(metafile = "test-data/fails_prescreening2.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Failed at pre-screening stage 2, fix it then screen again")

  app$set_inputs(resetbutton = "click") # Reset app

  # 7. Fail at final stage
  app$upload_file(datafile = "test-data/fails_allchecks.csv")
  app$upload_file(metafile = "test-data/fails_allchecks.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks but failed")

  app$set_inputs(resetbutton = "click") # Reset app

  # 9. Fail at final stage and ancillary
  app$upload_file(datafile = "test-data/fails_everything_ancillary.csv")
  app$upload_file(metafile = "test-data/fails_everything_ancillary.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks but failed")
  expect_true(app$get_value(export = "ancillary") > 0)

  app$set_inputs(resetbutton = "click") # Reset app

  # 10. Reset button (JSON 2)
  app$set_inputs(resetbutton = "click")

  app$expect_values(
    input = c("resetbutton", "screenbutton", "shinyjs-resettable-datafile", "shinyjs-resettable-metafile"),
    output = c("advisory_box", "all_tests", "ancillary_box", "failed_box", "passed_box", "data_cols", "data_rows", "data_size", "datafilename", "file_exists", "meta_rows", "meta_cols", "meta_size", "metafilename", "num_advisory_tests", "num_failed_tests", "progress_stage", "showresults", "sum_combined_tests", "sum_failed_tests", "sum_ignored_tests", "sum_passed_tests", "summary_text", "table_advisory_tests", "table_all_tests", "table_failed_tests", "testtime"),
    export = TRUE
  )
})
