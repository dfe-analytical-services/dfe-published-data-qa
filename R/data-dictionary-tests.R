# Check filter col_name entries against the data dictionary col_name listing
#
# @description All filter col_names should be correspond to entries in the data
# dictionary. This function takes a files meta data and validates the col_name
# entries against the data dictionary.
data_dictionary_filter_col_name_check <- function(meta) {
  # Collapse search terms for bad column names into regex term
  non_standard_col_names <- meta %>%
    select(col_name, filter_grouping_column) %>%
    pivot_longer(
      c(col_name, filter_grouping_column),
      values_to = "col_name"
    ) %>%
    filter(!(col_name %in% standard_col_names)) %>%
    pull(col_name)
  if (length(non_standard_col_names) == 0) {
    output <- list(
      "message" = "All col_names are consistent with the data dictionary.",
      "result" = "PASS"
    )
  } else {
    output <- list(
      "message" = paste0(
        "The column(s) '",
        paste(non_standard_col_names, collapse = "', '"), "' are not present in the data dictionary and should not be used as part of an API data set until resolved."
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
