library(shinytest2)

app <- AppDriver$new(name = "edge_cases", height = 846, width = 1445, load_timeout = 45 * 1000, timeout = 20 * 1000, wait = TRUE, expect_values_screenshot_args = FALSE)

# Wait until Shiny is not busy for 5ms
app$wait_for_idle(5)

# Single test_that function to save needing to restart the app each time
test_that("Edge cases", {
  # 1. YYYY-MM-DD dates do not crash the app
  app$upload_file(datafile = "test-data/date_format.csv")
  app$upload_file(metafile = "test-data/date_format.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")

  app$set_inputs(resetbutton = "click") # Reset app

  # 2. Missing geographies do not crash the app
  app$upload_file(datafile = "test-data/data_mandatory_cols.csv")
  app$upload_file(metafile = "test-data/data_mandatory_cols.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Failed at file validation stage, fix it then screen again")
  expect_true(app$get_value(export = "failed") == 1)

  app$set_inputs(resetbutton = "click") # Reset app

  # 3. Mix of filter groups
  app$upload_file(datafile = "test-data/filter_groups_mix.csv")
  app$upload_file(metafile = "test-data/filter_groups_mix.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")

  app$set_inputs(resetbutton = "click") # Reset app

  # 4. Hyphen warning appears as expected
  app$upload_file(datafile = "test-data/hyphen.csv")
  app$upload_file(metafile = "test-data/hyphen.meta.csv")
  app$set_inputs(screenbutton = "click")

  expect_true(app$get_value(export = "progress_message") == "Made it to the full screening checks and passed")
  expect_true(app$get_value(export = "advisory") > 0)

  # Extra check on the on the QA tabs
  # Doing in this script to reuse the hyphen file from the last test to save overall test time
  # Geog tab (JSON 1)
  app$set_inputs(trendy_tabs = "geogTab")
  app$set_inputs(geog_indicator_parameter = "full_time")
  app$set_inputs(submit_geographies = "click")

  app$expect_values(output = "geog_agg2")

  # Filters in 'What's in this file' (JSON 2)
  app$set_inputs(trendy_tabs = "obUnitTab")

  app$expect_values(output = "tables")
})
