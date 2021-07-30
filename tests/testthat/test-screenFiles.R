context("No filters")

test_that("noFiltersFinalStage", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/noFilters.csv")

  expect_equal(screeningOutput$progress_message, "Made it to the full screening checks and passed")
})

test_that("noFiltersPassEverything", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/noFilters.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("Passes everything")

test_that("PassEverything", {
  screeningOutput <- testOther("../../tests/shinytest/test-data/passes_everything.csv")

  expect_equal(screeningOutput$results %>% filter(!(result %in% c("PASS", "PASS WITH NOTE"))) %>% nrow(), 0)
})

context("Quoted blanks")

test_that("QuotedBlanks-overcompleted_cols", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "overcompleted_cols") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-region_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "region_code") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

context("Financial quarter and halves")

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

test_that("financialHalves", {
  screeningOutput <- testOther("../../tests/testthat/otherData/financial_half.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("Multiple filter groups")

test_that("multipleFilterGroupStripped", {
  screeningOutput <- testOther("../../tests/testthat/otherData/multiple_stripped_filter_groups.csv")

  expect_equal(screeningOutput$results %>% filter(test == "filter_group_stripped") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

context("z location")

test_that("zLocationCode", {
  screeningOutput <- testOther("../../tests/testthat/otherData/adding_z_locationCode.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("LAD within LA")

test_that("ladWithinLA", {
  screeningOutput <- testOther("../../tests/testthat/otherData/lad_within_la.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("Blank filter groups")

test_that("blankFilterGroupsMeta", {
  screeningOutput <- testOther("../../tests/testthat/otherData/blankFilterGroups.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("Passes na name")

test_that("passes_na_name", {
  screeningOutput <- testOther("../../tests/testthat/otherData/passes_na_name.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

context("School and provider scenarios")

test_that("prov_level_only", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/prov_level_only.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("sch_level_only", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_level_only.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("sch_mixed_levels", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_mixed_levels.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS WITH NOTE")
})

test_that("sch_prov", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_prov.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("sch_only_filter", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_only_filter.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ob_unit_meta") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
  expect_equal(screeningOutput$results %>% filter(test == "geographic_catch") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
  expect_equal(screeningOutput$results %>% filter(test == "overcompleted_cols") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("sch_many_filter", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_many_filter.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ob_unit_meta") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("sch_filter_grouped", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_filter_grouped.csv")
  
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("sch_filter_group", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_filter_grouped.csv")
  
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("not_sch_but_one_filter", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/not_sch_but_one_filter.csv")
  
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow() > 0, TRUE)
})
