context("preCheck1Functions")

pathStart <- "preCheck1/"

# invalid_meta_cols -------------------------------------------------------------------------------------------------------

test_that("invalid_meta_cols", {
  expect_equal(testIndividualTest(pathStart, "invalid_meta_cols"), "FAIL")
})

# meta_to_data_crosscheck -------------------------------------------------------------------------------------------------------

test_that("meta_to_data_crosscheck", {
  expect_equal(testIndividualTest(pathStart, "meta_to_data_crosscheck"), "FAIL")
})

# time_identifier -------------------------------------------------------------------------------------------------------

test_that("time_identifier", {
  expect_equal(testIndividualTest(pathStart, "time_identifier"), "FAIL")
})

# geographic_level -------------------------------------------------------------------------------------------------------

test_that("geographic_level", {
  expect_equal(testIndividualTest(pathStart, "geographic_level"), "FAIL")
})

# col_name_completed -------------------------------------------------------------------------------------------------------

test_that("col_name_completed", {
  expect_equal(testIndividualTest(pathStart, "col_name_completed"), "FAIL")
})

# duplicate_variable_names -------------------------------------------------------------------------------------------------------

test_that("duplicate_variable_names", {
  expect_equal(testIndividualTest(pathStart, "duplicate_variable_names"), "FAIL")
})
