# Data dictionary
read_data_dictionary <- function() {
  readr::read_csv("data/data-dictionary.csv")
}

data_dictionary_col_names <- function(data_dictionary) {
  data_dictionary |>
    dplyr::select(col_name, col_name_parent, col_type) |>
    tidyr::pivot_longer(
      c(col_name, col_name_parent),
      values_to = "col_name"
    ) |>
    dplyr::select(-name) |>
    dplyr::distinct() |>
    dplyr::arrange(col_type, col_name) |>
    dplyr::mutate(standard_col = TRUE)
}

data_dictionary_filter_items <- function(data_dictionary) {
  data_dictionary |>
    dplyr::select(
      col_type,
      colname_child = col_name,
      filteritem_child = filter_item,
      colname_parent = col_name_parent,
      filteritem_parent = filter_item_parent
    ) |>
    tidyr::pivot_longer(
      !col_type,
      names_to = c(".value", "hierarchy"),
      names_pattern = "(.*)_(.*)"
    ) |>
    dplyr::select(col_type, col_name = colname, filter_item = filteritem) |>
    dplyr::distinct() |>
    dplyr::arrange(col_type, col_name, filter_item) |>
    dplyr::mutate(standard_col = TRUE)
}


data_dictionary_match_columns <- function(meta, data_dictionary) {
  dd_col_names <- data_dictionary_col_names(data_dictionary)
  meta |>
    dplyr::select(col_type, col_name, filter_grouping_column) |>
    tidyr::pivot_longer(
      c(col_name, filter_grouping_column),
      values_to = "col_name"
    ) |>
    dplyr::select(-name) |>
    dplyr::filter(col_name != "") |>
    dplyr::left_join(dd_col_names, by = dplyr::join_by(col_type, col_name)) |>
    dplyr::filter(!is.na(col_name)) |>
    dplyr::distinct()
}


# Check filter col_name entries against the data dictionary col_name listing
#
# @description All filter col_names should be correspond to entries in the data
# dictionary. This function takes a files meta data and validates the col_name
# entries against the data dictionary.
data_dictionary_col_name_check <- function(meta) {
  # Collapse search terms for bad column names into regex term
  data_dictionary <- read_data_dictionary()
  non_standard_col_names <- data_dictionary_match_columns(meta, data_dictionary) |>
    dplyr::filter(is.na(standard_col))

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

data_dictionary_filter_item_check <- function(
    data,
    meta,
    group_field = "breakdown_topic",
    filter_field = "breakdown") {
  # First of all grab the filters to be scanned based on the meta data:
  dd <- read_data_dictionary()
  dd_cols_present <- data_dictionary_match_columns(meta, dd) |>
    dplyr::filter(!is.na(standard_col), col_type == "Filter")
  dd_filter_items <- data_dictionary_filter_items(dd)

  if (nrow(data_dictionary_cols_present) == 0) {
    output <- list(
      "message" = "No data dictionary columns found.",
      "result" = "SKIPPED"
    )
  } else {
    non_standard_filter_items <- data |>
      dplyr::select(all_of(dd_cols_present |> magrittr::extract2("col_name"))) |>
      dplyr::distinct() |>
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "col_name", values_to = "filter_item") |>
      dplyr::left_join(dd_filter_items, by = dplyr::join_by(col_name, filter_item)) |>
      dplyr::filter(is.na(standard_col))

    if (nrow(non_standard_filter_items) == 0) {
      output <- list(
        "message" = "All col_names are consistent with the data dictionary.",
        "result" = "PASS"
      )
    } else {
      non_standard_filter_items <- non_standard_filter_items |>
        dplyr::mutate(col_item_combo = paste0(col_name, "/", filter_item)) |>
        dplyr::pull(col_item_combo) |>
        paste(collapse = ", ")
      output <- list(
        "message" = paste(
          "The folling col_name/filter_item combination(s) are not present in the data dictionary",
          "and should not be used as part of an API data set until resolved:\n",
          non_standard_filter_items
        ),
        "result" = "WARNING"
      )
    }
  }
}
