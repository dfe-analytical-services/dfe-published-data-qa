# Variables checked in these tests are set in the `R/knownVariables.R` file
#
# Checks that will fail if we change the rows in the geography matrix
# We refer to the values in it throughout the repo by row and col number so any changes may affect other tests
# Known tests that particularly rely on this are all in the mainTests.R script:
# - overcompleted_cols
# - na_geography
# - na_geography_code
# - whitespace_filters

test_that("All field names are in snake case", {
  expect_equal(
    length(
      unique(
        unlist(
          str_split(
            gsub("[a-z0-9]|_", "", acceptable_observational_units),
            ""
          ),
          use.names = FALSE
        )
      )
    ),
    0
  )
})

test_that("geographic_level is the first col in the matrix", {
  expect_equal(
    colnames(geography_matrix)[1],
    "geographic_level"
  )
})

test_that("Field cols are as expected in the matrix", {
  expect_equal(
    colnames(geography_matrix)[2:4],
    c("code_field", "name_field", "code_field_secondary")
  )
})

test_that("School, Provider, Institution and Planning area are rows 14-17 in order", {
  expect_equal(
    geography_matrix[14:17, 1],
    c("School", "Provider", "Institution", "Planning area")
  )
})

test_that("First 7 rows are as expected", {
  expect_equal(
    geography_matrix[1:7, 1],
    c(
      "National", "Regional", "Local authority", "Local authority district", "RSC region",
      "Parliamentary constituency", "Local skills improvement plan area"
    )
  )
})
