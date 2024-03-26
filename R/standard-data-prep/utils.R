# Dependencies for this script ====
library(readr)
library(dplyr)
library(stringr)

# Lookup table for shorthand to levels we care about ====

open_geog_shorthands <- c("WD", "PCON", "LAD", "UTLA", "LSIP")
geographic_level <- c("Ward", "Parliamentary consistuency", "Local authority district", "Local authority", "Local skills improvment plan area")
name_column <- paste0(c("ward", "pcon", "lad", "la", "lsip"), "_name")
code_column <- paste0(c("ward", "pcon", "lad", "new_la", "lsip"), "_code")

shorthand_lookup <- data.frame(
  open_geog_shorthands,
  geographic_level,
  name_column,
  code_column
)

# Helper functions ====

#' Tidy a downloaded lookup file from the Open Geography Portal
#'
#' Takes a file from the open geography portal and tidies it ready for appending to an existing lookup
#'
#' @param open_geography_file Lookup file sourced from
#'      https://geoportal.statistics.gov.uk/search?q=LUP_WPC&sort=Date%20Created%7Ccreated%7Cdesc
#'      e.g. data/downloaded_source_data/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_to_Upper_Tier_L.csv
#' @param output_lookup
#'
#' @return

tidy_downloaded_lookup <- function(
    open_geography_file) {
  # Read in the downloaded open geography file --------------------------------
  message("Reading in new data from: ", open_geography_file)
  new_data <- read_csv(open_geography_file, show_col_types = FALSE)

  # Extract the year from columns ---------------------------------------------
  new_year <- names(new_data) %>%
    as.data.frame() %>%
    # Filter to only geog cols
    filter(grepl(paste(open_geog_shorthands, collapse = "|"), .)) %>%
    # Remove the geog shorthands and CD / NM
    pull(.) %>%
    str_remove_all(paste(c(open_geog_shorthands, "CD", "NM"), collapse = "|")) %>%
    # Pull out the year
    unique()

  # Check there is only one year available ------------------------------------
  if (length(new_year) != 1) {
    stop("There appears to be either zero or multiple years of data in the downloaded lookup, the function doesn't know which year to pick")
  }

  #' Function to rename columns using the shorthand_lookup table made in
  #' R/standard-data-prep/utils.R
  #'
  #' @param col_name single column name to be updated based on the shorthand
  #' lookup table
  #'
  #' @return string for new column name if a match was found, if no match found
  #' then the original name is returned
  generate_new_name <- function(col_name) {
    # Take the prefix and check it exists
    prefix <- str_extract(col_name, "^[A-Z]*")
    if (prefix %in% open_geog_shorthands) {
      # Take the suffix
      suffix <- str_sub(col_name, start = -2, end = -1)

      # Replace with either the name or code column as appropriate
      if (suffix == "NM") {
        new_name <- shorthand_lookup %>%
          filter(open_geog_shorthands == prefix) %>%
          pull(name_column)
      } else {
        new_name <- shorthand_lookup %>%
          filter(open_geog_shorthands == prefix) %>%
          pull(code_column)
      }

      message("Renaming ", col_name, " to ", new_name)
      return(new_name) # Return replaced name
    } else {
      message("No match found for ", col_name, ", returning original name")
      return(col_name) # Keep original name if no match
    }
  }

  # Apply the function to rename columns
  names(new_data) <- unlist(lapply(names(new_data), generate_new_name))

  # Add columns showing years for the codes
  new_lookup <- new_data %>%
    distinct() %>%
    mutate(
      first_available_year_included = paste0("20", new_year),
      most_recent_year_included = paste0("20", new_year)
    ) %>%
    select(-ObjectId)

  # Extra tidy up for separating LAs / LADs
  # Not 100% on the logic but it seems to be filtering / sifting between LAs and LADs - is that right Rich?
  if ("new_la_code" %in% names(new_lookup)) {
    new_lookup1 <- new_lookup %>%
      mutate(
        la_name = if_else(
          grepl("E11", new_la_code) | grepl("E12", new_la_code) | grepl("E13", new_la_code),
          lad_name,
          la_name
        ),
        new_la_code = if_else(
          grepl("E11", new_la_code) | grepl("E12", new_la_code) | grepl("E13", new_la_code),
          lad_code,
          new_la_code
        )
      )
  }

  return(new_lookup)
}

#' Overwrite the existing lookup file by appending new data
#'
#' Take a tidied file produced by the tidy_downloaded_lookup function and append to existing lookup file
#'
#' @param new_lookup data fram of new lookup table, usually the output of tidy_downloaded_lookup
#' @param existing_data_file filepath to existing lookup you want to update
#'
#' @return message confirming if the existing CSV has been overwritten

write_updated_lookup <- function(
    new_lookup,
    existing_data_file) {
  # Start by reading the existing lookup from the repo
  message("Reading data from existing file: ", existing_data_file)
  existing_lookup <- read_csv(existing_data_file, show_col_types = FALSE)

  # Work out the geography columns we care about in the new lookup
  cols_to_join_by <- names(new_lookup)[!names(new_lookup) %in% c("first_available_year_included", "most_recent_year_included")]

  # Append the new lookup
  updated_lookup <- existing_lookup %>%
    rbind(new_lookup) %>%
    # Then condense the rows, rewriting the first and last years for each row
    summarise(
      first_available_year_included = min(first_available_year_included),
      most_recent_year_included = max(most_recent_year_included),
      .by = cols_to_join_by
    )

  # Update the existing lookup
  message("Writing updated lookup to: ", output_lookup)
  tryCatch(
    {
      write.csv(updated_lookup, row.names = FALSE)
      message("...", output_lookup, " successfully updated")
    },
    error = function(msg) {
      message("Issue writing lookup file. Try looking at the code within the `write_updated_lookup()` function and running in the console separately.")
    }
  )
}
