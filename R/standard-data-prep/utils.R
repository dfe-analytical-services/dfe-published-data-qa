# Dependencies for this script ================================================
library(readr)
library(dplyr)
library(stringr)

# Helpful variables ===========================================================
# Names of time columns
time_cols <- c(
  "first_available_year_included",
  "most_recent_year_included"
)

# Create a lookup table for shorthand to levels we care about
open_geog_shorthands <- c("WD", "PCON", "LAD", "UTLA", "LSIP")
name_column <- paste0(c("ward", "pcon", "lad", "la", "lsip"), "_name")
code_column <- paste0(c("ward", "pcon", "lad", "new_la", "lsip"), "_code")

open_geog_shorthand_lookup <- data.frame(
  open_geog_shorthands,
  name_column,
  code_column
)

# Create a vector of potential expected cols in lookups
potential_cols <- c(
  time_cols,
  unlist(open_geog_shorthand_lookup$name_column, use.names = FALSE),
  unlist(open_geog_shorthand_lookup$code_column, use.names = FALSE)
)

# Helper functions ============================================================

#' Tidy a downloaded lookup file from the Open Geography Portal
#'
#' Takes a file from the open geography portal and tidies it ready for
#' appending to an existing lookup
#'
#' @param open_geography_file lookup file downloaded from Open Geography Portal,
#' e.g. "data/downloaded_source_data/my_newly_downloaded_file.csv"
#' @param shorthand_lookup data frame that gives the conversion from the Open
#' Geography Portal's shorthands to our column names
#'
#' @return a data frame of a tidied lookup file
tidy_downloaded_lookup <- function(
    open_geography_file,
    shorthand_lookup) {
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
  #' R/standard-data-prep/update-geography-lookups.R
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
    )

  # Remove ObjectId if it exists
  if (suppressWarnings(!is.null(new_lookup$ObjectId))) {
    new_lookup <- new_lookup %>% select(-ObjectId)
  }

  # Extra tidy up for separating LAs / LADs
  # This bit's to deal with LAs and UTLAs not being quite the same thing.
  # The parent UTLA for LADs in metropolitan counties (E11, e.g. Manchester),
  # and Inner or Outer London (E13) are the Metropolitan counties
  # and Inner / Outer London.
  # But in these cases, the LA for our purposes is the LAD itself.

  # So this is using the LAD as it's own parent LA if it's in a metropolitan
  # county or in London and taking the UTLA otherwise.

  if ("new_la_code" %in% names(new_lookup)) {
    new_lookup <- new_lookup %>%
      mutate(
        la_name = if_else(
          grepl("E11", new_la_code) | grepl("E13", new_la_code),
          lad_name,
          la_name
        ),
        new_la_code = if_else(
          grepl("E11", new_la_code) | grepl("E13", new_la_code),
          lad_code,
          new_la_code
        )
      )
  }

  # Strip out excess white space from name columns
  new_lookup <- new_lookup %>%
    mutate(across(ends_with("_name"), ~ str_replace_all(.x, "\\s+", " "))) %>%
    # Also strip out leading and trailing whitespace for belts and braces
    mutate(across(everything(), ~ str_trim(.x)))

  return(new_lookup)
}

#' Overwrite the existing lookup file by appending new data
#'
#' Take a tidied file, likely produced by the tidy_downloaded_lookup function
#' and either append to existing lookup file or write a new lookup file if
#' one doesn't exist at the given file path
#'
#' If a lookup file already exists it will automatically only use the columns
#' present in that file
#'
#' @param new_lookup data frame of new lookup table,
#' usually the output of tidy_downloaded_lookup
#' @param lookup_filepath file path to existing lookup you want to update,
#' or the path for the new lookup to be written to
#'
#' @return message confirming if the CSV file has been successfully written

write_updated_lookup <- function(
    new_lookup,
    lookup_filepath) {
  # If a lookup file exists already read it in
  if (file.exists(lookup_filepath)) {
    message("Reading data from existing file: ", lookup_filepath)
    existing_lookup <- read_csv(lookup_filepath, show_col_types = FALSE)

    # Filter the new lookup to only have the same columns
    new_lookup <- new_lookup %>% select(all_of(names(existing_lookup)))
  } else {
    existing_lookup <- data.frame()
    message("No existing lookup found at: ", lookup_filepath)
  }

  # Work out the geography columns we care about in the new lookup
  cols_to_join_by <- names(new_lookup)[!names(new_lookup) %in% time_cols]

  # Append the new lookup
  updated_lookup <- existing_lookup %>%
    rbind(new_lookup) %>%
    # Then condense the rows, rewriting the first and last years for each row
    summarise(
      first_available_year_included = min(first_available_year_included),
      most_recent_year_included = max(most_recent_year_included),
      .by = all_of(cols_to_join_by)
    )

  # Final tidy up of the output file ==========================================
  # Select only columns that we expect
  # Start with any possible column from the shorthand table
  expected_columns <- potential_cols %>%
    # Filter to only the ones that exist in the new file too
    intersect(names(updated_lookup))

  # Select the columns we expect
  updated_lookup <- updated_lookup[, expected_columns]

  # Pull out code columns
  sorting_cols <- names(updated_lookup %>% select(ends_with("_code")))

  # Order the file by year and then code columns
  updated_lookup %>% arrange(desc(most_recent_year_included), !!!sorting_cols)

  # Update the existing lookup
  message("Writing new lookup to: ", lookup_filepath)
  tryCatch(
    {
      write.csv(updated_lookup, file = paste(lookup_filepath), row.names = FALSE)
      message("...", lookup_filepath, " successfully written")
    },
    error = function(msg) {
      message("Issue writing lookup file. Try looking at the code within the `write_updated_lookup()` function and running in the console separately.")
    }
  )
}
