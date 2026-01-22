find_duplicates <- function(data, meta) {
  geography_fields <- intersect(
    names(data),
    c(geography_dataframe$code_field, geography_dataframe$name_field)
  )
  data_filters <-
    meta |>
    dplyr::filter(col_type == "Filter") |>
    dplyr::pull(col_name) |>
    c(
      destinations_ks4_meta |>
        dplyr::filter(filter_grouping_column != "") |>
        dplyr::pull(filter_grouping_column)
    ) |>
    unique()
  fields <- c(mandatory_fields, geography_fields, data_filters)
  duplicates <- data |>
    dplyr::summarise(duplication = n(), .by = all_of(fields)) |>
    dplyr::filter(duplication > 1) |>
    dplyr::left_join(data) |>
    dplyr::arrange(!!!rev(c(mandatory_fields, filters)))
  message("Found ", nrow(duplicates), "duplicates")
  duplicates
}
