test_that("noFiltersFinalStage", {
  screeningOutput <- testOther("../../tests/testthat/test-data/noFilters.csv")

  expect_equal(screeningOutput$progress_message, "Made it to the full screening checks and passed")
})

test_that("noFiltersPassEverything", {
  screeningOutput <- testOther("../../tests/testthat/test-data/noFilters.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("PassEverything", {
  screeningOutput <- testOther("../../tests/testthat/test-data/passes_everything.csv")

  expect_equal(screeningOutput$results %>% filter(!(result %in% c("PASS", "PASS WITH NOTE"))) %>% nrow(), 0)
})

test_that("QuotedBlanks-overcompleted_cols", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "overcompleted_cols") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-old_la_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "la_combinations") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})

test_that("QuotedBlanks-region_code", {
  screeningOutput <- testOther("../../tests/testthat/otherData/quoted_blank_geographies.csv")

  expect_equal(screeningOutput$results %>% filter(test == "region_combinations") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
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

test_that("financialHalves", {
  screeningOutput <- testOther("../../tests/testthat/otherData/financial_half.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("multipleFilterGroupStripped", {
  screeningOutput <- testOther("../../tests/testthat/otherData/multiple_stripped_filter_groups.csv")

  expect_equal(screeningOutput$results %>% filter(test == "filter_group_stripped") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("blankFilterGroupsMeta", {
  screeningOutput <- testOther("../../tests/testthat/otherData/blankFilterGroups.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("zLocationCode", {
  screeningOutput <- testOther("../../tests/testthat/otherData/adding_z_locationCode.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 1)
})

test_that("ladWithinLA", {
  screeningOutput <- testOther("../../tests/testthat/otherData/lad_within_la.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("multipleLevelsName", {
  screeningOutput <- testOther("../../tests/testthat/otherData/name_multiple_levels.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("passes_na_name", {
  screeningOutput <- testOther("../../tests/testthat/otherData/passes_na_name.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("outside_of_england_region", {
  screeningOutput <- testOther("../../tests/testthat/otherData/outside_of_england_region.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("regional_blanks", {
  screeningOutput <- testOther("../../tests/testthat/otherData/regional_blanks.csv")

  expect_equal(screeningOutput$results %>% filter(test == "region_combinations") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow() > 0, TRUE)
})

test_that("prov_level_only", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/prov_level_only.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("sch_level_only", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_level_only.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("sch_mixed_levels", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_mixed_levels.csv")

  expect_equal(screeningOutput$results %>% filter(test == "ignored_rows") %>% pull(result) %>% unlist(use.names = FALSE), "PASS WITH NOTE")
  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
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
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_filter_group.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("sch_with_trust_cols", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/school_with_trust.csv")

  expect_equal(screeningOutput$results %>% filter(test == "other_geography_duplicates") %>% pull(result) %>% unlist(use.names = FALSE), "IGNORE")
  expect_equal(screeningOutput$results %>% filter(test == "other_geography_code_duplicates") %>% pull(result) %>% unlist(use.names = FALSE), "IGNORE")
})

test_that("not_sch_but_one_filter", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/not_sch_but_one_filter.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow() > 0, TRUE)
})

test_that("prov_level_only_dupes", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/prov_level_only_dupes.csv")

  expect_equal(screeningOutput$results %>% filter(test == "duplicate_rows") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("sch_level_missing_col", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/sch_level_missing_col.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow() > 0, TRUE)
})

test_that("prov_level_diff_codes", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/prov_level_diff_codes.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})


test_that("prov_level_diff_names", {
  screeningOutput <- testOther("../../tests/testthat/sch_prov/prov_level_diff_names.csv")

  expect_equal(screeningOutput$results %>% filter(result == "FAIL") %>% nrow(), 0)
})

test_that("blank_meta_label_notNA_still_fails", {
  screeningOutput <- testOther("../../tests/testthat/test-data/label_blank_notNA.csv")

  expect_equal(screeningOutput$results %>% filter(test == "label") %>% pull(result) %>% unlist(use.names = FALSE), "FAIL")
})

test_that("Can handle incorrect provider cols", {
  expect_no_error(screeningOutput <- testOther("../../tests/testthat/otherData/provider_col_incorrect.csv"))
})

test_that("Can handle missing region_name", {
  expect_no_error(screeningOutput <- testOther("../../tests/testthat/otherData/missing_region_name.csv"))
})

test_that("all valid indicator units do pass", {
  screeningOutput <- testOther("../../tests/testthat/otherData/indicator_units_should_pass.csv")

  expect_equal(screeningOutput$results %>% filter(test == "indicator_unit_validation") %>% pull(result) %>% unlist(use.names = FALSE), "PASS")
})
