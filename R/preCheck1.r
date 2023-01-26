# preCheck1 -------------------------------------
# Pre-checks stage 1 functions

preCheck1 <- function(datafile, metafile) {
  as_tibble(t(rbind(
    cbind(
      invalid_meta_cols(metafile), # active test
      meta_to_data_crosscheck(datafile, metafile), # active test
      time_identifier(datafile), # active test
      geographic_level(datafile), # active test
      col_name_completed(metafile), # active test
      duplicate_variable_names(datafile) # active test
    ),
    "stage" = "preCheck1",
    "test" = c(activeTests$`R/preCheck1.r`)
  )))
}

# invalid_meta_cols -------------------------------------
# check for invalid columns in the metadata file

invalid_meta_cols <- function(meta) {
  if (length(setdiff(names(meta), mandatory_meta_cols)) == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "There are no invalid columns in the metadata file."
    )
  } else {
    if (length(setdiff(names(meta), mandatory_meta_cols)) == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste(setdiff(names(meta), mandatory_meta_cols), "is an invalid column in the metadata file.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following are invalid columns in the metadata file: '", paste(setdiff(names(meta), mandatory_meta_cols), collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}

# meta_to_data_crosscheck -------------------------------------
# For each variable in the metadata check they appear in the data file

meta_to_data_crosscheck <- function(data, meta) {
  column_crosscheck_check <- function(i) {
    if ((i %in% names(data)) == FALSE) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  filter_groups <- meta %>%
    filter(filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  pre_result <- stack(sapply(c(meta$col_name, filter_groups), column_crosscheck_check))

  missing_variables <- filter(pre_result, values == "FAIL") %>% pull(ind)

  if ("FAIL" %in% pre_result$values) {
    output <- list(
      "result" = "FAIL",
      "message" = paste0("The following variables were found in the metadata file, but could not be found in the data file: '", paste(missing_variables, collapse = "', '"), "'.")
    )
  } else {
    output <- list(
      "result" = "PASS",
      "message" = "All variables from the metadata were found in the data file."
    )
  }

  return(output)
}

# time_identifier -------------------------------------
# checking the time identifier values are valid

time_identifier <- function(data) {
  time_identifier_check <- function(i) {
    if ((i %in% acceptable_time_identifiers) == FALSE) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  identifiers_present <- as.character(unique(data$time_identifier))

  pre_result <- stack(sapply(identifiers_present, time_identifier_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "result" = "PASS",
      "message" = "The time_identifier values are all valid."
    )
  } else {
    invalid_identifiers <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(invalid_identifiers) == 1) {
      if (invalid_identifiers == "") {
        output <- list(
          "result" = "FAIL",
          "message" = "At least one of the time_identifier values is blank."
        )
      } else {
        output <- list(
          "result" = "FAIL",
          "message" = paste0("The following invalid time_identifier was found in the file: '", paste(invalid_identifiers, collapse = "', '"), "'.")
        )
      }
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following invalid time_identifiers were found in the file: '", paste(invalid_identifiers, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}

# geographic_level -------------------------------------
# Do we have acceptable values for the geographic level

geographic_level <- function(data) {
  level_validity_check <- function(i) {
    if ((i %in% acceptable_levels) == FALSE) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  pre_result <- stack(sapply(unique(data$geographic_level), level_validity_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "result" = "PASS",
      "message" = "The geographic_level values are all valid."
    )
  } else {
    invalid_levels <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(invalid_levels) == 1) {
      if (invalid_levels == "") {
        output <- list(
          "result" = "FAIL",
          "message" = "At least one of the geographic_level values is blank."
        )
      } else {
        output <- list(
          "result" = "FAIL",
          "message" = paste0("The following invalid geographic level was found in the file: '", paste(invalid_levels, collapse = "', '"), "'.")
        )
      }
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following invalid geographic levels were found in the file: ", paste(invalid_levels, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}

# col_name_completed -------------------------------------
# is col_name completed for every row

col_name_completed <- function(meta) {
  completed_col_names <- meta %>%
    filter(col_name != "") %>%
    nrow()
  blank_col_names <- nrow(meta) - completed_col_names

  if (blank_col_names == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "The col_name column is completed for every row in the metadata."
    )
  } else {
    if (blank_col_names == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("There is a col_name missing in ", paste(blank_col_names), " row of the metadata file.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("There are col_name values missing in ", paste(blank_col_names), " rows of the metadata file.")
      )
    }
  }

  return(output)
}

# duplicate_variable_names -------------------------------------
# Checking datafile for duplicate columns

duplicate_variable_names <- function(data) {
  duplicate_variable_names <- names(data)[duplicated(names(data))]

  if (length(duplicate_variable_names) == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "All variable names are unique in the datafile."
    )
  } else {
    if (length(duplicate_variable_names) == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following variable name is duplicated in the data file: '", paste(duplicate_variable_names), "'.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following variable names are duplicated in the data file: '", paste0(duplicate_variable_names, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}
