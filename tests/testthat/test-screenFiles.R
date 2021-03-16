context("screenFiles")

test_that("noFiltersFinalStage", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/noFilters.csv")

  expect_equal(screeningOutput$progress_message, "Made it to the full screening checks and passed")
})

test_that("noFiltersPassEverything", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/noFilters.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("PassEverything", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/passes_everything.csv")

  expect_equal(screeningOutput$results %>% filter(result != "PASS") %>% nrow(), 0)
})

test_that("QuotedBlanks-overcompleted_cols", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "overcompleted_cols") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-old_la_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "old_la_code") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-region_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "region_code") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("financialQuarterValid", {
  screeningOutput <- testOther("../../tests/testthat/otherData/financial_quarter.csv")

  expect_equal(screeningOutput$results %>% filter(test == "time_identifier") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("financialQuarterMix", {
  screeningOutput <- testOther("../../tests/testthat/otherData/financial_quarter.csv")

  expect_equal(screeningOutput$results %>% filter(test == "time_identifier_mix") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("financialQuarterDigits", {
  screeningOutput <- testOther("../../tests/testthat/otherData/financial_quarter.csv")

  expect_equal(screeningOutput$results %>% filter(test == "time_period") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("multipleFilterGroupStripped", {
  screeningOutput <- testOther("../../tests/testthat/otherData/multiple_stripped_filter_groups.csv")

  expect_equal(screeningOutput$results %>% filter(test == "filter_group_stripped") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("zLocationCode", {
  screeningOutput <- testOther("../../tests/testthat/otherData/adding_z_locationCode.csv")

  expect_equal(screeningOutput$results %>% filter(result != "PASS") %>% nrow(), 0)
})

test_that("ladWithinLA", {
  screeningOutput <- testOther("../../tests/testthat/otherData/lad_within_la.csv")
  
  expect_equal(screeningOutput$results %>% filter(result != "PASS") %>% nrow(), 0)
})
