# This is a standalone function not run by the screener itself.
# I'm using it to quickly create a list of column names and filter items from
# a set of data files and sort them into a data-dictionary structure with
# flags as to whether they're currently in the data dictionary or not.

api_data_checker <- function(files) {
  entries <- data.frame(
    col_name = NA,
    col_type = NA,
    filter_item = NA,
    col_name_parent = NA,
    filter_item_parent = NA
  )
  for (file in files) {
    data <- vroom::vroom(file)
    meta <- vroom::vroom(gsub(".csv", ".meta.csv", file))
    print(meta)
    if ("Filter" %in% meta$col_type) {
      filters <- meta |>
        dplyr::filter(col_type == "Filter") |>
        dplyr::select("col_name", "filter_grouping_column")
      for (i in 1:nrow(filters)) {
        columns <- c(
          filters$col_name[i],
          filters$filter_grouping_column[i]
        )
        filter_info <- data |>
          dplyr::select(all_of(columns[!is.na(columns)])) |>
          dplyr::distinct() |>
          dplyr::mutate(col_name = paste0(filters$col_name[i]))
        if (!is.na(filters$filter_grouping_column[i])) {
          filter_info <- filter_info |>
            dplyr::mutate(
              col_name_parent = paste0(filters$filter_grouping_column[i])
            ) |>
            dplyr::rename(filter_item_parent = filters$filter_grouping_column[i])
        } else {
          filter_info <- filter_info |>
            dplyr::mutate(filter_item_parent = "", col_name_parent = "")
        }
        entries <- entries |>
          dplyr::bind_rows(
            filter_info |>
              dplyr::select(
                col_name,
                filter_item = filters$col_name[i],
                col_name_parent,
                filter_item_parent
              ) |>
              dplyr::mutate(col_type = "Filter")
          )
      }
    }
    entries <- entries |>
      dplyr::bind_rows(
        meta |> dplyr::filter(col_type == "Indicator") |> dplyr::select(col_name, col_type)
      ) |>
      dplyr::mutate(across(everything(), ~ dplyr::if_else(is.na(.x), "", .x)))
  }
  return(
    entries |>
      dplyr::filter(!is.na(col_name)) |>
      dplyr::select(
        col_name,
        col_type,
        filter_item,
        col_name_parent,
        filter_item_parent,
      ) |>
      dplyr::arrange(
        col_type,
        col_name,
        col_name_parent,
        filter_item_parent,
        filter_item
      ) |>
      dplyr::distinct()
  )
}

non_dd_rows <- function(listing) {
  dd <- vroom::vroom("data/data-dictionary.csv") |>
    dplyr::select(
      col_name,
      col_type,
      filter_item,
      col_name_parent,
      filter_item_parent
    ) |>
    dplyr::mutate(across(everything(), ~ dplyr::if_else(is.na(.x), "", .x)))
  listing |>
    dplyr::anti_join(dd)
}

col_name_validation <- function(listing) {
  too_long <- listing |>
    dplyr::filter(stringr::str_length(col_name) > 50 | stringr::str_length(col_name_parent) > 50)
  if (nrow(too_long) > 0) {
    warning("Found col_names with too many chars")
    print(too_long)
  } else {
    message("All col_names within contraint")
  }
}

checker_example_run <- function(dir) {
  #  dir <- "../../offline-data/ks2_attainment/2025-september/Publication\ files/"
  files <- list.files(dir)
  files <- paste0(dir, files[!grepl("meta", files)])
  listing <- api_data_checker(files)
  listing |> col_name_validation()
  return(non_dd_rows(listing))
}
