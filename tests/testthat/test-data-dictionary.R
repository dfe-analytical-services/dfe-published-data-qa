dd <- read_csv("../../data/data-dictionary.csv")

test_that("Check for any problems on data dictionary read-in (e.g. blank extra columns)", {
  expect_equal(
    dd |>
      problems() |>
      nrow(),
    0
  )
})


test_that("Duplicate row test", {
  expect_equal(
    dd |>
      dplyr::summarise(
        count = dplyr::n(),
        .by = c(
          "col_name",
          "filter_item",
          "col_name_parent",
          "filter_item_parent"
        )
      ) |>
      dplyr::filter(count > 1) |>
      nrow(),
    0
  )
})

test_that("Check for non-standard characters", {
  expect_equal(
    dd |>
      dplyr::filter(if_any(everything(), ~ grepl("[$^&@'#~]", .))) |>
      nrow(),
    0
  )
})
