las <- read.csv("../../data/las.csv")
lads <- read.csv("../../data/lads.csv")

test_that("Unitary authorities (E06) match between LA and LAD look-ups", {
  expect_equal(
    las |>
      dplyr::select(new_la_code, la_name) |>
      dplyr::filter(grepl("E06", new_la_code)) |>
      nrow(),
    lads |>
      dplyr::select(lad_code, lad_name) |>
      dplyr::filter(grepl("E06", lad_code)) |>
      nrow()
  )
})

test_that("Metropolitan boroughs (E08) match between LA and LAD look-ups", {
  expect_equal(
    las |>
      dplyr::select(new_la_code, la_name) |>
      dplyr::filter(grepl("E08", new_la_code)) |>
      nrow(),
    lads |>
      dplyr::select(lad_code, lad_name) |>
      dplyr::filter(grepl("E08", lad_code)) |>
      nrow()
  )
})
