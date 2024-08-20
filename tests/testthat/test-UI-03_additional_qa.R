library(shinytest2)

app <- AppDriver$new(name = "additional_qa", height = 846, width = 1445, load_timeout = 45 * 1000, timeout = 20 * 1000, wait = TRUE, expect_values_screenshot_args = FALSE)

# Wait until Shiny is not busy for 5ms
app$wait_for_idle(5)

# Single test_that function to save needing to restart the app each time
test_that("Additional QA tabs", {
  # 1. Preview table
  app$upload_file(datafile = "test-data/passes_everything.csv")
  app$upload_file(metafile = "test-data/passes_everything.meta.csv")
  app$set_inputs(screenbutton = "click")
  app$set_inputs(trendy_tabs = "previewTab")

  app$expect_values(output = c("meta_table", "data_preview"))

  # 2. Do the file info tables populate as expected
  app$set_inputs(trendy_tabs = "obUnitTab")

  app$expect_values(output = c("geog_time_perms2", "tables", "indicators", "suppressed_cell_count_table"))

  # 3. Explore indicators doesn't break with no arguments
  app$set_inputs(trendy_tabs = "indicatorsTab")
  app$set_inputs(submit = "click")

  app$expect_values(output = "table_list")

  # 4. Explore indicators doesn't break with just indicators
  app$set_inputs(ind_parameter = "num_schools")
  app$set_inputs(submit = "click")

  app$expect_values(output = "table_list")

  # 5. Explore indicators doesn't break with just geography
  app$set_inputs(ind_parameter = character(0))
  app$set_inputs(geog_parameter = c("National", "Regional", "Local authority", "Local authority district"))
  app$set_inputs(submit = "click")

  app$expect_values(output = "table_list")

  # 6. Explore indicators works as expected
  app$set_inputs(ind_parameter = c("num_schools", "enrolments"))
  app$set_inputs(submit = "click")

  app$expect_values(output = c("t_1", "t_2", "t_3", "t_4", "t_5", "t_6", "t_7", "t_8"))

  # 7. Outlier check doesn't break with no arguments
  app$set_inputs(trendy_tabs = "outliersTab")
  app$set_inputs(comptime_parameter = "201718")
  app$set_inputs(submit_outlier = "click")

  app$expect_values(output = "table_outlier_list")

  # 8. Outlier check doesn't break with just indicator
  app$set_inputs(outlier_indicator_parameter = "num_schools")
  app$set_inputs(submit_outlier = "click")

  app$wait_for_idle(50) # this test seems weirdly flaky for unknown reasons

  app$expect_values(output = "table_outlier_list")

  # 9. Outlier check doesn't run with same time periods
  app$set_inputs(outlier_indicator_parameter = character(0))
  app$set_inputs(comptime_parameter = "201617")
  app$set_inputs(submit_outlier = "click")

  app$expect_values(output = "table_outlier_list")

  # 10. Outlier check runs as expected
  app$set_inputs(outlier_indicator_parameter = c("num_schools", "enrolments"))
  app$set_inputs(threshold_setting = 5)
  app$set_inputs(submit_outlier = "click")

  app$wait_for_idle(50) # this test seems weirdly flaky for unknown reasons

  app$expect_values(output = c("to_1", "to_2"))

  # 11. Geography check runs as expected
  app$set_inputs(trendy_tabs = "geogTab")
  app$set_inputs(geog_indicator_parameter = "num_schools")
  app$set_inputs(submit_geographies = "click")

  app$wait_for_idle(50) # this test seems weirdly flaky for unknown reasons

  app$expect_values(output = "geog_agg2")
})
