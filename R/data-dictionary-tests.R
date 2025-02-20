# Data dictionary
data_dictionary <- function() {
  readr::read_csv("data/data-dictionary.csv")
}


# Check filter col_name entries against the data dictionary col_name listing
#
# @description All filter col_names should be correspond to entries in the data
# dictionary. This function takes a files meta data and validates the col_name
# entries against the data dictionary.
data_dictionary_filter_col_name_check <- function(meta) {
  # Collapse search terms for bad column names into regex term
  data_dictionary <- data_dictionary()
  dd_col_names <- data_dictionary |>
    dplyr::select(col_type, col_name, col_parent_name) |>
    tidyr::pivot_longer(
      c(col_name, col_parent_name),
      values_to = "col_name"
    ) |>
    dplyr::select(-name) |>
    dplyr::distinct() |>
    dplyr::arrange(col_type, col_name) |>
    dplyr::mutate(standard_col = TRUE)

  non_standard_col_names <- meta |>
    dplyr::select(col_type, col_name, filter_grouping_column) |>
    tidyr::pivot_longer(
      c(col_name, filter_grouping_column),
      values_to = "col_name"
    ) |>
    dplyr::left_join(dd_col_names, by = dplyr::join_by(col_type, col_name)) |>
    dplyr::filter(!is.na(col_name), is.na(standard_col)) |>
    dplyr::distinct()

  if (nrow(non_standard_col_names) == 0) {
    output <- list(
      "message" = "All col_names are consistent with the data dictionary.",
      "result" = "PASS"
    )
  } else {
    non_standard_indicators <- non_standard_col_names |>
      dplyr::filter(col_type == "Indicator") |>
      dplyr::pull("col_name") |>
      paste(collapse = ", ")
    non_standard_filters <- non_standard_col_names |>
      dplyr::filter(col_type == "Filter") |>
      dplyr::pull("col_name") |>
      paste(collapse = ", ")
    output <- list(
      "message" = paste(
        "The folling column(s) are not present in the data dictionary",
        "and should not be used as part of an API data set until resolved:\n",
        "Indicators:",
        ifelse(non_standard_indicators != "", non_standard_indicators, "PASSED"),
        "\n",
        "Filters:",
        ifelse(non_standard_filters != "", non_standard_filters, "PASSED")
      ),
      "result" = "WARNING"
    )
  }
  return(output)
}

standard_filters_scan_groups <- function(
    data,
    standard_group_entries,
    standard_item_entries,
    group_alias_grepl = NULL,
    group_field = "breakdown_topic",
    filter_field = "breakdown") {
  data_fields <- data %>%
    select(all_of(c(group_field, filter_field))) %>%
    distinct()
  relevant_entries <- data_fields %>%
    filter(!!sym(group_field) %in% standard_group_entries) %>%
    rbind(
      data_fields %>%
        filter(!!sym(filter_field) %in% standard_item_entries)
    )
}
