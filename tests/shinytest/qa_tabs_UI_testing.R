app <- ShinyDriver$new("../../")
app$snapshotInit("qa_tabs_UI_testing", screenshot = FALSE)


# # # 1. Do the right tabs appear for files with multiple geographies ---------
# app$uploadFile(datafile = "test-data/passes_everything.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$uploadFile(metafile = "test-data/passes_everything.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$setInputs(screenbutton = "click")
# # app$snapshot()


# 1. Do the metadata preview tables populate as expected ------------------
app$uploadFile(datafile = "test-data/passes_everything.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$uploadFile(metafile = "test-data/passes_everything.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(screenbutton = "click")
app$setInputs(trendy_tabs = "previewTab")
app$snapshot(list(output = c("meta_table", "data_preview")))


# 2. Do the file info tables populate as expected -------------------------
app$setInputs(trendy_tabs = "obUnitTab")
app$snapshot(list(output = c("geog_time_perms2", "table_1", "indicators", "suppressed_cell_count_table")))


# 3. Explore indicators doesn't break with no arguments -------------------
app$setInputs(trendy_tabs = "indicatorsTab")
app$setInputs(submit = "click")
app$snapshot(list(output = "table_list"))

# 4. Explore indicators doesn't break with just indicators ----------------
app$setInputs(ind_parameter = "num_schools")
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_list"))

# 5. Explore indicators doesn't break with just geography -----------------
app$setInputs(ind_parameter = character(0))
app$setInputs(geog_parameter = c("National", "Regional", "Local authority", "Local authority district"))
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_list"))

# 6. Explore indicators works as expected ---------------------------------
app$setInputs(ind_parameter = c("num_schools", "enrolments"))
app$setInputs(submit = "click")
Sys.sleep(2)
app$snapshot(list(output = c("t_1", "t_2", "t_3", "t_4", "t_5", "t_6", "t_7", "t_8")))


# 7. Outlier check doesn't break with no arguments ------------------------
app$setInputs(trendy_tabs = "outliersTab")
app$setInputs(comptime_parameter = "201718")
app$setInputs(submit_outlier = "click")
app$snapshot(list(output = "table_outlier_list"))


# 8. Outlier check doesn't break with just indicator ----------------------
app$setInputs(outlier_indicator_parameter = "num_schools")
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_outlier_list"))


# 9. Outlier check doesn't run with same time periods --------------------
app$setInputs(outlier_indicator_parameter = character(0))
app$setInputs(comptime_parameter = "201617")
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = "table_outlier_list"))


# 10. Outlier check runs as expected --------------------------------------
app$setInputs(outlier_indicator_parameter = c("num_schools", "enrolments"))
app$setInputs(threshold_setting = 5)
app$setInputs(submit_outlier = "click")
Sys.sleep(2)
app$snapshot(list(output = c("to_1", "to_2")))


# 11. Geography check runs as expected ------------------------------------
app$setInputs(trendy_tabs = "geogTab")
app$setInputs(geog_indicator_parameter = "num_schools")
app$setInputs(submit_geographies = "click")
Sys.sleep(2)
app$snapshot(list(output = "geog_agg2"))

# 12. Hyphen warning appears as expected -----------------------------------

app$setInputs(resetbutton = "click")
app$uploadFile(datafile = "test-data/hyphen.csv")
app$uploadFile(metafile = "test-data/hyphen.meta.csv")
app$setInputs(screenbutton = "click")
app$snapshot(list(output = "geog_agg2"))

# # 12. Correct tabs populate for single-level geography file ---------------
# app$setInputs(trendy_tabs = "tab1")
# app$setInputs(resetbutton = "click")
# app$uploadFile(datafile = "test-data/passes_everything_ancillary.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$uploadFile(metafile = "test-data/passes_everything_ancillary.meta.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$setInputs(screenbutton = "click")
# app$snapshot(list(output = "trendy_tabs"))
