app <- ShinyDriver$new("../../")
app$snapshotInit("UI_tests", screenshot = FALSE)

# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
Sys.sleep(1)
app$snapshot()

# 2 Passes everything -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything.csv")
app$uploadFile(metafile = "test-data/passes_everything.meta.csv")
Sys.sleep(2.8)
app$setInputs(screenbutton = "click",timeout_=8.0e4)
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 3 - Passes everything advisory -------------------------------------------------------------------------------------------------------------------
app$uploadFile(datafile = "test-data/passes_everything_advisory.csv")
app$uploadFile(metafile = "test-data/passes_everything_advisory.meta.csv")
Sys.sleep(2.8)
app$setInputs(screenbutton = "click",timeout_=8.0e4)
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

# 11. Do the metadata preview tables populate as expected ------------------
app$uploadFile(datafile = "test-data/passes_everything.csv")
app$uploadFile(metafile = "test-data/passes_everything.meta.csv")
app$setInputs(screenbutton = "click",timeout_=8.0e4)
app$setInputs(trendy_tabs = "previewTab")
app$snapshot(list(output = c("meta_table", "data_preview")))

# 12. Do the file info tables populate as expected -------------------------
app$setInputs(trendy_tabs = "obUnitTab")
app$snapshot(list(output = c("geog_time_perms2", "table_1", "indicators", "suppressed_cell_count_table")))

# 13. Explore indicators doesn't break with no arguments -------------------
app$setInputs(trendy_tabs = "indicatorsTab")
app$setInputs(submit = "click")
app$snapshot(list(output = "table_list"))

# 14. Explore indicators doesn't break with just indicators ----------------
app$setInputs(ind_parameter = "num_schools")
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_list"))

# 15. Explore indicators doesn't break with just geography -----------------
app$setInputs(ind_parameter = character(0))
app$setInputs(geog_parameter = c("National", "Regional", "Local authority", "Local authority district"))
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_list"))

# 16. Explore indicators works as expected ---------------------------------
app$setInputs(ind_parameter = c("num_schools", "enrolments"))
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = c("t_1", "t_2", "t_3", "t_4", "t_5", "t_6", "t_7", "t_8")))

# 17. Outlier check doesn't break with no arguments ------------------------
app$setInputs(trendy_tabs = "outliersTab")
app$setInputs(comptime_parameter = "201718")
app$setInputs(submit_outlier = "click")
app$snapshot(list(output = "table_outlier_list"))

# 18. Outlier check doesn't break with just indicator ----------------------
app$setInputs(outlier_indicator_parameter = "num_schools")
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_outlier_list"))

# 19. Outlier check doesn't run with same time periods --------------------
app$setInputs(outlier_indicator_parameter = character(0))
app$setInputs(comptime_parameter = "201617")
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_outlier_list"))

# 20. Outlier check runs as expected --------------------------------------
app$setInputs(outlier_indicator_parameter = c("num_schools", "enrolments"))
app$setInputs(threshold_setting = 5)
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = c("to_1", "to_2")))

# 21. Geography check runs as expected ------------------------------------
app$setInputs(trendy_tabs = "geogTab")
app$setInputs(geog_indicator_parameter = "num_schools")
app$setInputs(submit_geographies = "click")
Sys.sleep(2)
app$snapshot(list(output = "geog_agg2"))

# 22. Hyphen warning appears as expected -----------------------------------

app$setInputs(resetbutton = "click")
app$uploadFile(datafile = "test-data/hyphen.csv")
app$uploadFile(metafile = "test-data/hyphen.meta.csv")
app$setInputs(screenbutton = "click")
app$setInputs(trendy_tabs = "geogTab")
app$setInputs(geog_indicator_parameter = "full_time")
app$setInputs(submit_geographies = "click")
app$snapshot(list(output = "geog_agg2"))

# 23. Hyphen works for filters on What's in this file ----------------------

app$setInputs(trendy_tabs = "obUnitTab")
app$snapshot(list(output = c("table_1", "table_2")))

app$setInputs(resetbutton = "click")

# 24 - YYYY-MM-DD dates do not crash the app  -------------------------------------
app$uploadFile(datafile = "test-data/date_format.csv")
app$uploadFile(metafile = "test-data/date_format.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 25 - Missing geographies do not crash the app  -------------------------------------
app$uploadFile(datafile = "test-data/data_mandatory_cols.csv")
app$uploadFile(metafile = "test-data/data_mandatory_cols.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(items = list(output = c("progress_stage", "table_all_tests")))

app$setInputs(resetbutton = "click")

# 26 - Mix of filter groups -------------------------------------
app$uploadFile(datafile = "test-data/data_mandatory_cols.csv")
app$uploadFile(metafile = "test-data/data_mandatory_cols.meta.csv")
app$setInputs(screenbutton = "click")
app$setInputs(trendy_tabs = "obUnitTab")
app$snapshot(list(output = c("table_1", "table_2", "table_3", "table_4")))

app$setInputs(resetbutton = "click")
