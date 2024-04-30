pathStart <- "fileValidation/"

# filename_spaces --------------------------------------------------------------------------------------------------

test_that("filename_spaces", {
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "filename spaces.csv"), paste0(pathStart, "filename spaces.meta.csv"), "data_filename_spaces"), "FAIL")
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "filename spaces.csv"), paste0(pathStart, "filename spaces.meta.csv"), "meta_filename_spaces"), "FAIL")
})

# special_characters-+#'@ --------------------------------------------------------------------------------------------------

test_that("filename_spaces", {
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "special_characters-+#'@.csv"), paste0(pathStart, "special_characters-+#'@.meta.csv"), "data_filename_special_characters"), "FAIL")
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "special_characters-+#'@.csv"), paste0(pathStart, "special_characters-+#'@.meta.csv"), "meta_filename_special_characters"), "FAIL")
})

# naming_convention --------------------------------------------------------------------------------------------------

test_that("naming_convention", {
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "naming_convention.csv"), paste0(pathStart, "naming_convention_meta.csv"), "naming_convention"), "FAIL")
})

# rows_to_cols --------------------------------------------------------------------------------------------------

test_that("rows_to_cols", {
  expect_equal(testIndividualTest(pathStart, "rows_to_cols"), "FAIL")
})

# data_empty_rows --------------------------------------------------------------------------------------------------

test_that("data_empty_rows", {
  expect_equal(testIndividualTest(pathStart, "data_empty_rows"), "FAIL")
})

# meta_empty_rows --------------------------------------------------------------------------------------------------

test_that("meta_empty_rows", {
  expect_equal(testIndividualTest(pathStart, "meta_empty_rows"), "FAIL")
})

# data_empty_cols --------------------------------------------------------------------------------------------------

test_that("data_empty_cols", {
  expect_equal(testIndividualTest(pathStart, "data_empty_cols"), "FAIL")
})

# data_mandatory_cols --------------------------------------------------------------------------------------------------

test_that("data_mandatory_cols", {
  expect_equal(testIndividualTest(pathStart, "data_mandatory_cols"), "FAIL")
})

# meta_mandatory_cols --------------------------------------------------------------------------------------------------

test_that("meta_mandatory_cols", {
  expect_equal(testIndividualTest(pathStart, "meta_mandatory_cols"), "FAIL")
})
