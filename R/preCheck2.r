# preCheck2 -------------------------------------
# Pre-checks stage 2 functions

preCheck2 <- function(data_character, meta_character, datafile, metafile) {
  as_tibble(t(rbind(cbind(
    time_identifier_mix(datafile), # active test
    geography_level_present(datafile), # active test
    data_variable_spaces(datafile), # active test
    ob_unit_meta(metafile), # active test
    filter_level(datafile, metafile), # active test
    utf8(data_character, meta_character), # active test
    col_type(metafile) # active test
  ),
  "stage" = "preCheck2",
  "test" = c(activeTests$`R/preCheck2.r`)
  )))
}

# time_identifier_mix -------------------------------------
# print the unique time_identifiers for conceptual checking

time_identifier_mix <- function(data) {
  base_identifier <- data$time_identifier[1]

  possible_levels <- list("Autumn and spring term", "Calendar year", "Financial year", "Academic year", "Tax year", "Reporting year", terms, weeks, months)

  baseLevelCheck <- function(possible_level) {
    if (base_identifier %in% possible_level) {
      return(possible_level)
    }
  }

  base_level <- unlist(lapply(possible_levels, baseLevelCheck), use.names = FALSE)

  if (any(is.na(factor(unique(data$time_identifier), base_level))) == TRUE) {
    output <- list(
      "message" = paste0("The datafile is mixing incompatable time identifiers. <br> - Allowable values with '", paste(base_identifier), "' present, are: '", paste(base_level, sep = "', '"), "'. <br> - See the <a href='https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values' target='_blank'>guidance on time values</a> if you are unsure."),
      "result" = "FAIL"
    )
  } else {
    if (length(unique(data$time_identifier)) == 1) {
      output <- list(
        "message" = "There is a single, valid time_identifer.",
        "result" = "PASS"
      )
    } else {
      output <- list(
        "message" = "The time_identifier values are mixed appropriately.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# geography_level_present -------------------------------------
# Do we have the right columns for the geographic level

geography_level_present <- function(data) {
  if (all(data$geographic_level == "National")) {
    output <- list(
      "message" = "There is only National level data in the file.",
      "result" = "IGNORE"
    )
  } else {
    expected_cols <- function(i) {
      # if a geographic level is present, then this returns the expected cols from the pre-defined geography_matrix

      if (i[1] %in% data$geographic_level) {
        return(i[2:4])
      }
    }

    # filter out the non table tool rows / cols from geography matrix
    geography_present <- geography_matrix[1:12, 1:4]

    missing_cols <- unlist(apply(geography_present, 1, expected_cols)) %>%
      .[!is.na(.)] %>%
      setdiff(names(data))

    if (length(missing_cols) == 0) {
      output <- list(
        "message" = "The geography columns are present as expected for the geographic_level values in the file.",
        "result" = "PASS"
      )
    } else {
      if (length(missing_cols) == 1) {
        output <- list(
          "message" = paste0("Given that the following geographic_level values are present: '", paste(unique(data$geographic_level), collapse = "', '"), "'; <br> - the following column is missing from the file: '", paste(missing_cols, collapse = "', '"), "'."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("Given that the following geographic_level values are present: '", paste(unique(data$geographic_level), collapse = "', '"), "'; <br> - the following columns are missing from the file: '", paste(missing_cols, collapse = "', '"), "'."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# data_variable_spaces -------------------------------------
# Checking datafile for spaces in variable names

data_variable_spaces <- function(data) {
  data_spaces_check <- function(i) {
    if (any(grepl("\\s", i))) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  pre_result <- stack(sapply(names(data), data_spaces_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "message" = "There are no spaces in the variable names in the datafile.",
      "result" = "PASS"
    )
  } else {
    failed_cols <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(failed_cols) == 1) {
      output <- list(
        "message" = paste0("The following variable name has at least one space that needs removing: '", paste(failed_cols), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following variable names each have at least one space that needs removing: '", paste(failed_cols, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# ob_unit_meta -------------------------------------
# check if any observational units are in the metadata

ob_unit_meta <- function(meta) {
  ob_unit_meta_check <- function(i) {
    if (i %in% meta$col_name) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  pre_result <- stack(sapply(acceptable_observational_units, ob_unit_meta_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "message" = "No observational units have been included in the metadata file.",
      "result" = "PASS"
    )
  } else {
    ob_units_in_meta <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(ob_units_in_meta) == 1) {
      output <- list(
        "message" = paste0("The following observational unit needs removing from the metadata file: '", paste(ob_units_in_meta, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following observational units need removing from the metadata file: '", paste(ob_units_in_meta, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# filter_level -------------------------------------
# filters in the metadata file should have more than one value - flag when they only have one

filter_level <- function(data, meta) {
  mfilters <- filter(meta, col_type == "Filter")
  dfilters <- select(data, mfilters$col_name)

  if (ncol(dfilters) == 0) {
    output <- list(
      "message" = "There are no filters in your data to test.",
      "result" = "IGNORE"
    )
  } else {
    filter_level_check <- function(i) {
      if ((length(unique(data[[i]]))) < 2) {
        return("FAIL")
      } else {
        return("PASS")
      }
    }

    pre_result <- stack(sapply(names(dfilters), filter_level_check))

    if (all(pre_result$values == "PASS")) {
      output <- list(
        "message" = "All filters have two or more levels.",
        "result" = "PASS"
      )
    } else {
      single_level_filters <- filter(pre_result, values == "FAIL") %>% pull(ind)

      if (length(single_level_filters) == 1) {
        output <- list(
          "message" = paste0("There are fewer than two levels in '", paste(single_level_filters, collapse = "', '"), "'. <br> - This should be removed from the metadata."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("There are fewer than two levels in the following filters: '", paste(single_level_filters, collapse = "', '"), "'. <br> - These should be removed from the metadata."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# utf8 -------------------------------------
# check for invalidly encoded symbols (should be utf-8)

utf8 <- function(data, meta) {
  data_valid_result <- c(unlist(data, use.names = FALSE), names(data)) %>% validUTF8()
  meta_valid_result <- c(unlist(meta, use.names = FALSE), names(meta)) %>% validUTF8()

  number_data_invalid <- sum(!data_valid_result)
  number_meta_invalid <- sum(!meta_valid_result)

  if (number_data_invalid == 0 && number_meta_invalid == 0) {
    output <- list(
      "message" = "The data and metadata files are both recognised as using UTF-8 encoding.",
      "result" = "PASS"
    )
  } else {
    if (number_data_invalid > 0 && number_meta_invalid > 0) {
      output <- list(
        "message" = paste0("Neither of the data and metadata files are using UTF-8 encoding. <br> - See the ", "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#data_format' target='_blank'>guidance on how to do this</a>", " if you are unsure."),
        "result" = "FAIL"
      )
    } else {
      if (number_data_invalid > 0) {
        output <- list(
          "message" = paste0("The data file is not using UTF-8 encoding. <br> - See the ", "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#data_format' target='_blank'>guidance on how to do this</a>", " if you are unsure."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The metadata file is not using UTF-8 encoding. <br> - See the ", "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#data_format' target='_blank'>guidance on how to do this</a>", " if you are unsure."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# col_type -------------------------------------
# col_type - is this one of 'Filter' or 'Indicator'

col_type <- function(meta) {
  invalid_types <- meta %>%
    filter(
      col_type != "Filter",
      col_type != "Indicator"
    ) %>%
    pull(col_type)

  if (length(invalid_types) == 0) {
    output <- list(
      "message" = "col_type is always 'Filter' or 'Indicator'.",
      "result" = "PASS"
    )
  } else {
    if (length(invalid_types) == 1) {
      output <- list(
        "message" = paste0("The following invalid col_type value was found in the metadata file: '", paste0(invalid_types, collapse = "', '"), "'. <br> - col_type must always be either 'Filter' or 'Indicator', and cannot be blank."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following invalid col_type values were found in the metadata file: '", paste0(invalid_types, collapse = "', '"), "'. <br> - col_type must always be either 'Filter' or 'Indicator', and cannot be blank."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}
