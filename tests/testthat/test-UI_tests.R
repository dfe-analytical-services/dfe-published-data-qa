library(shinytest2)

test_that("Migrated shinytest test: UI_tests.R", {
  app <- AppDriver$new(load_timeout = 640000)

  # 1. Does it load  -------------------------------------------------------------------------------------------------------------------
  message("\nRunning test 1 - load test")
  Sys.sleep(1) 
  app$expect_values()

  # 2 Passes everything -------------------------------------------------------------------------------------------------------------------
  message("Running test 2 - passes!")
  app$upload_file(datafile = "test-data/passes_everything.csv")
  app$upload_file(metafile = "test-data/passes_everything.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 12000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests"))

  app$set_inputs(resetbutton = "click")

  # 3 - Passes everything advisory -------------------------------------------------------------------------------------------------------------------
  message("Running test 3 - passes, advisory")
  app$upload_file(datafile = "test-data/passes_everything_advisory.csv")
  app$upload_file(metafile = "test-data/passes_everything_advisory.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 12000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests"))

  app$set_inputs(resetbutton = "click")

  # 4 - Passes everything ancillary  -------------------------------------------------------------------------------------------------------------------
  message("Running test 4 - passes, ancilary")
  app$upload_file(datafile = "test-data/passes_everything_ancillary.csv")
  app$upload_file(metafile = "test-data/passes_everything_ancillary.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 5 - Fail at prescreening stage 2  -------------------------------------------------------------------------------------------------------------------
  message("Running test 5 - fail at pre-screening")
  app$upload_file(datafile = "test-data/fails_prescreening2.csv")
  app$upload_file(metafile = "test-data/fails_prescreening2.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 6 - Fail at pre-screening stage 1  -------------------------------------------------------------------------------------------------------------------
  message("Running test 6 - fail at pre-screening")
  app$upload_file(datafile = "test-data/fails_prescreening1.csv")
  app$upload_file(metafile = "test-data/fails_prescreening1.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 7 - Fail at final stage  -------------------------------------------------------------------------------------------------------------------
  message("Running test 7 - fail all checks")
  app$upload_file(datafile = "test-data/fails_allchecks.csv")
  app$upload_file(metafile = "test-data/fails_allchecks.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 8 - Fail initial file validation  -------------------------------------------------------------------------------------------------------------------
  message("Running test 8 - fail file validation")
  app$upload_file(datafile = "test-data/fails_file validation.csv")
  app$upload_file(metafile = "test-data/fails_file validation.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 9 - Fail at final stage and ancillary  -------------------------------------------------------------------------------------------------------------------
  message("Running test 9 - fails everything ancillary")
  app$upload_file(datafile = "test-data/fails_everything_ancillary.csv")
  app$upload_file(metafile = "test-data/fails_everything_ancillary.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests",
    "fail_box", "passed_box", "advisory_box", "ancillary_box"))

  app$set_inputs(resetbutton = "click")

  # 10 - Reset button -------------------------------------------------------------------------------------------------------------------
  message("Running test 10 - reset after failing")
  app$expect_values(input = c("resetbutton", "screenbutton", "shinyjs-resettable-datafile",
    "shinyjs-resettable-metafile"), output = c("advisory_box",
    "all_tests", "ancillary_box", "failed_box", "passed_box",
    "data_cols", "data_rows", "data_size", "datafilename", "file_exists",
    "meta_rows", "meta_cols", "meta_size", "metafilename", "num_advisory_tests",
    "num_failed_tests", "progress_stage", "showresults", "sum_combined_tests",
    "sum_failed_tests", "sum_ignored_tests", "sum_passed_tests",
    "summary_text", "table_advisory_tests", "table_all_tests",
    "table_failed_tests", "testtime"), export = TRUE)

  # 11. Do the metadata preview tables populate as expected ------------------
  message("Running test 11 - preview table")
  app$upload_file(datafile = "test-data/passes_everything.csv")
  app$upload_file(metafile = "test-data/passes_everything.meta.csv")
  app$set_inputs(screenbutton = "click")
  Sys.sleep(3.2)
  app$set_inputs(trendy_tabs = "previewTab", timeout_ = 16000)
  app$expect_values(output = c("meta_table", "data_preview"))

  # 12. Do the file info tables populate as expected -------------------------
  message("Running test 12 - file info tables")
  app$set_inputs(trendy_tabs = "obUnitTab", timeout_ = 16000)
  app$expect_values(output = c("geog_time_perms2", "tables", "indicators",
    "suppressed_cell_count_table"))

  # 13. Explore indicators doesn't break with no arguments -------------------
  message("Running test 13 - test indicator choice with no arguments")
  app$set_inputs(trendy_tabs = "indicatorsTab")
  app$set_inputs(submit = "click", timeout_ = 16000)
  app$expect_values(output = "table_list")

  # 14. Explore indicators doesn't break with just indicators ----------------
  message("Running test 14 - explore indicators doesn't break with just indicators")
  app$set_inputs(ind_parameter = "num_schools")
  app$set_inputs(submit = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "table_list")

  # 15. Explore indicators doesn't break with just geography -----------------
  message("Running test 15 - explore indicators doesn't break with just geography")
  app$set_inputs(ind_parameter = character(0))
  app$set_inputs(geog_parameter = c("National", "Regional", "Local authority",
    "Local authority district"))
  app$set_inputs(submit = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "table_list")

  # 16. Explore indicators works as expected ---------------------------------
  message("Running test 16 - explore indicators works as expected")
  app$set_inputs(ind_parameter = c("num_schools", "enrolments"))
  app$set_inputs(submit = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("t_1", "t_2", "t_3", "t_4", "t_5",
    "t_6", "t_7", "t_8"))

  # 17. Outlier check doesn't break with no arguments ------------------------
  message("Running test 17 - outlier check doesn't break due to no arguments")
  app$set_inputs(trendy_tabs = "outliersTab")
  app$set_inputs(comptime_parameter = "201718")
  app$set_inputs(submit_outlier = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "table_outlier_list")

  # 18. Outlier check doesn't break with just indicator ----------------------
  message("Running test 18 - outlier check doesn't break with only indicator")
  app$set_inputs(outlier_indicator_parameter = "num_schools")
  app$set_inputs(submit_outlier = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "table_outlier_list")

  # 19. Outlier check doesn't run with same time periods --------------------
  message("Running test 19 - outlier check doesn't run with same time periods")
  app$set_inputs(outlier_indicator_parameter = character(0))
  app$set_inputs(comptime_parameter = "201617")
  app$set_inputs(submit_outlier = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "table_outlier_list")

  # 20. Outlier check runs as expected --------------------------------------
  message("Running test 20 - outlier check runs as expected")
  app$set_inputs(outlier_indicator_parameter = c("num_schools",
    "enrolments"))
  app$set_inputs(threshold_setting = 5)
  app$set_inputs(submit_outlier = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("to_1", "to_2"))

  # 21. Geography check runs as expected ------------------------------------
  message("Running test 21 - geography check runs as expected")
  app$set_inputs(trendy_tabs = "geogTab")
  app$set_inputs(geog_indicator_parameter = "num_schools")
  app$set_inputs(submit_geographies = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = "geog_agg2")

  # 22. Hyphen warning appears as expected -----------------------------------
  message("Running test 22 - hyphen warning appears as expected")
  app$set_inputs(resetbutton = "click")
  app$upload_file(datafile = "test-data/hyphen.csv")
  app$upload_file(metafile = "test-data/hyphen.meta.csv")
  app$set_inputs(screenbutton = "click")
  Sys.sleep(3.2)
  app$set_inputs(trendy_tabs = "geogTab")
  app$set_inputs(geog_indicator_parameter = "full_time")
  app$set_inputs(submit_geographies = "click")
  Sys.sleep(3.2)
  app$expect_values(output = "geog_agg2")

  # 23. Hyphen works for filters on What's in this file ----------------------
  message("Running test 23 - hyphen works for filters on What's in this file")

  app$set_inputs(trendy_tabs = "obUnitTab")
  app$expect_values(output = c("tables"))

  app$set_inputs(resetbutton = "click")

  # 24 - YYYY-MM-DD dates do not crash the app  -------------------------------------
  message("Running test 24 - YYYY-MM-DD dates do not crash the app")
  app$upload_file(datafile = "test-data/date_format.csv")
  app$upload_file(metafile = "test-data/date_format.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests"))

  app$set_inputs(resetbutton = "click")

  # 25 - Missing geographies do not crash the app  -------------------------------------
  message("Running test 25 - missing geographies do not crash the app")

  app$upload_file(datafile = "test-data/data_mandatory_cols.csv")
  app$upload_file(metafile = "test-data/data_mandatory_cols.meta.csv")
  app$set_inputs(screenbutton = "click", timeout_ = 16000)
  Sys.sleep(3.2)
  app$expect_values(output = c("progress_stage", "table_all_tests"))

  app$set_inputs(resetbutton = "click")

  # 26 - Mix of filter groups -------------------------------------
  message("Running test 26 - mix of filter groups does not crash the app")
  app$upload_file(datafile = "test-data/data_mandatory_cols.csv")
  app$upload_file(metafile = "test-data/data_mandatory_cols.meta.csv")
  app$set_inputs(screenbutton = "click")
  app$set_inputs(trendy_tabs = "obUnitTab")
  app$expect_values(output = c("tables"))

  app$set_inputs(resetbutton = "click")
})
