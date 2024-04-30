pathStart <- "preCheck2/"

# time_identifier_mix -------------------------------------------------------------------------------------------------------

test_that("time_identifier_mix", {
  expect_equal(testIndividualTest(pathStart, "time_identifier_mix"), "FAIL")
})

# geography_level_present -------------------------------------------------------------------------------------------------------

test_that("geography_level_present", {
  expect_equal(testIndividualTest(pathStart, "geography_level_present"), "FAIL")
})

# data_variable_spaces -------------------------------------------------------------------------------------------------------

test_that("data_variable_spaces", {
  expect_equal(testIndividualTest(pathStart, "data_variable_spaces"), "FAIL")
})

# ob_unit_meta -------------------------------------------------------------------------------------------------------

test_that("ob_unit_meta", {
  expect_equal(testIndividualTest(pathStart, "ob_unit_meta"), "FAIL")
})

# filter_level  -------------------------------------------------------------------------------------------------------

test_that("filter_level", {
  expect_equal(testIndividualTest(pathStart, "filter_level"), "FAIL")
})

# utf8 -------------------------------------------------------------------------------------------------------

test_that("utf8", {
  expect_equal(testIndividualTest(pathStart, "utf8"), "FAIL")
})


# col_type -------------------------------------------------------------------------------------------------------

test_that("col_type", {
  expect_equal(testIndividualTest(pathStart, "col_type"), "FAIL")
})

# col_type -------------------------------------------------------------------------------------------------------

test_that("col_type_blank", {
  expect_equal(testIndividualTestSeparate(paste0(pathStart, "col_type_blank.csv"), paste0(pathStart, "col_type_blank.meta.csv"), "col_type"), "FAIL")
})

# time_validation -------------------------------------------------------------------------------------------------------

test_that("time_validation", {
  expect_equal(testIndividualTest(pathStart, "time_validation"), "FAIL")
})
