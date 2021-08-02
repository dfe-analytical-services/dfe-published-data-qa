# mainTests -------------------------------------
# main tests functions

mainTests <- function(data_character, meta_character, datafile, metafile) {
  as_tibble(t(rbind(cbind(
    variable_snake_case(datafile), # active test
    variable_start_letter(datafile), # active test
    duplicate_rows(datafile, metafile), # active test
    data_to_meta_crosscheck(datafile, metafile), # active test
    total(datafile, metafile), # active test
    observational_total(datafile, metafile), # active test
    null(data_character, meta_character), # active test
    suppression_symbols(datafile, metafile), # active test
    no_data_symbols(datafile), # active test
    blanks_filters(data_character, meta_character), # active test
    blanks_indicators(data_character, meta_character), # active test
    time_period(datafile), # active test
    time_period_six(datafile), # active test
    three_years(datafile), # active test
    region_for_la(datafile), # active test
    region_for_lad(datafile), # active test
    geography_level_completed(datafile), # active test
    region_col_completed(datafile), # active test
    overcompleted_cols(datafile, metafile), # active test
    ignored_rows(datafile), # active test
    la_combinations(datafile), # active test
    region_combinations(datafile), # active test
    country_combinations(datafile), # active test
    school_urn_duplicates(datafile), # active test
    school_laestab_duplicates(datafile), # active test
    other_geography_duplicates(datafile), # active test
    other_geography_code_duplicates(datafile), # active test
    na_geography(datafile), # active test
    na_geography_code(datafile), # active test
    col_name_duplicate(metafile), # active test
    col_name_spaces(metafile), # active test
    label(metafile), # active test
    duplicate_label(metafile), # active test
    geographic_catch(metafile), # active test
    filter_hint(metafile), # active test
    filter_group(metafile), # active test
    filter_group_match(datafile, metafile), # active test
    filter_group_level(datafile, metafile), # active test
    filter_group_not_filter(metafile), # active test
    filter_group_duplicate(metafile), # active test
    whitespace_filters(datafile, metafile), # active test
    indicator_grouping(metafile), # active test
    filter_group_stripped(data_character, meta_character), # active test
    indicator_group_stripped(meta_character), # active test
    indicator_unit(metafile), # active test
    indicator_unit_validation(metafile), # active test
    indicator_dp(metafile), # active test
    indicator_dp_validation(metafile), # active test
    indicator_dp_completed(metafile) # active test
  ),
  "stage" = "mainTests",
  "test" = c(activeTests$`R/mainTests.r`)
  )))
}

# variable_snake_case -------------------------------------
# Checking datafile for whether the variable names are following snake case

variable_snake_case <- function(data) {
  present_special_characters <- unique(unlist(str_split(gsub("[a-z0-9]|_|\\s", "", names(data)), ""), use.names = FALSE))

  if (length(present_special_characters) == 0) {
    output <- list(
      "message" = "The variable names in the data file follow the snake_case convention.",
      "result" = "PASS"
    )
  } else {
    if (length(present_special_characters) == 1) {
      output <- list(
        "message" = paste0("The following invalid character was found in the variable names of the data file: ", paste0("'", present_special_characters, collapse = "', '"), "'. <br> - Variable names should follow the snake_case convention and only contain lowercase letters, underscores or numbers."),
        "result" = "ADVISORY"
      )
    } else {
      output <- list(
        "message" = paste0("The following invalid characters were found in the variable names of the data file: ", paste0("'", present_special_characters, collapse = "', '"), "'. <br> - Variable names should follow the snake_case convention and only contain lowercase letters, underscores or numbers."),
        "result" = "ADVISORY"
      )
    }
  }

  return(output)
}

# variable_start_letter ----------------------------------
# Checking that no variables start with a lowercase letter

variable_start_letter <- function(data) {
  start_character_validation <- function(variable) {
    return(grepl("^(a-z])", variable))
  }

  invalid_variables <- sapply(names(data), start_character_validation) %>%
    stack() %>%
    filter(values == TRUE) %>%
    pull(ind)

  if (length(invalid_variables) == 0) {
    output <- list(
      "message" = "All variables in the data file start with a lowercase letter.",
      "result" = "PASS"
    )
  } else {
    if (length(invalid_variables) == 1) {
      output <- list(
        "message" = paste0("The following variable name starts with a character that isn't a lowercase letter: '", paste0(invalid_variables), "'. <br> - All variable names should start with a lowercase letter."),
        "result" = "ADVISORY"
      )
    } else {
      output <- list(
        "message" = paste0("The following variable names start with a character that isn't a lowercase letter: '", paste0(invalid_variables, collapse = "', '"), "'. <br> - All variable names should start with a lowercase letter."),
        "result" = "ADVISORY"
      )
    }
  }

  return(output)
}

# duplicate_rows -------------------------------------
# Checking datafile for duplicate rows across ob. units and filters

duplicate_rows <- function(data, meta) {
  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)

  filter_groups <- meta %>%
    filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  present_obUnits_filters <- intersect(c(acceptable_observational_units, filters, filter_groups), names(data))

  if (nrow(data %>% distinct(geographic_level)) == 1 &&
    data$geographic_level[1] %in% geography_matrix[13:14, 1]
  ) {
    dupes <- suppressMessages(data %>%
      filter(geographic_level != geography_matrix[15, 1]) %>%
      filter(geographic_level != geography_matrix[16, 1]) %>%
      select(present_obUnits_filters) %>%
      get_dupes())
    
    if (nrow(dupes) > 0) {
      output <- list(
        "message" = paste("There are", cs_num(nrow(dupes)), "duplicate rows in the data file. <br> - Note that", geography_matrix[15, 1], " and ", geography_matrix[16, 1]), "level rows were not included in this test."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste("There are no duplicate rows in the data file. <br> - Note that", paste0(geography_matrix[15, 1], " and ", geography_matrix[16, 1]), "level rows were not included in this test."),
        "result" = "PASS"
      )
    }
  } else {
    dupes <- suppressMessages(data %>%
      filter(geographic_level != geography_matrix[13, 1]) %>%
      filter(geographic_level != geography_matrix[14, 1]) %>%
      filter(geographic_level != geography_matrix[15, 1]) %>%
      filter(geographic_level != geography_matrix[16, 1]) %>%
      select(present_obUnits_filters) %>%
      get_dupes())
    
    if (nrow(dupes) > 0) {
      output <- list(
        "message" = paste("There are", cs_num(nrow(dupes)), "duplicate rows in the data file. <br> - Note that", paste0(geography_matrix[13, 1], ", ", geography_matrix[14, 1], ", ", geography_matrix[15, 1], " and ", geography_matrix[16, 1]), "level rows were not included in this test."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste("There are no duplicate rows in the data file. <br> - Note that", paste0(geography_matrix[13, 1], ", ", geography_matrix[14, 1], ", ", geography_matrix[15, 1], " and ", geography_matrix[16, 1]), "level rows were not included in this test."),
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# data_to_meta_crosscheck -------------------------------------
# List those in the data file that aren't in the metadata (or observational units, or variables with only one level)

data_to_meta_crosscheck <- function(data, meta) {
  single_level <- function(i) {
    if (length(unique(data[[i]])) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  single_level_cols <- stack(sapply(names(data), single_level)) %>%
    filter(values == TRUE) %>%
    pull(ind) %>%
    as.character()

  present_ob_units <- c(
    intersect(acceptable_observational_units, names(data)),
    names(data)[grepl(potential_ob_units_regex, names(data), ignore.case = TRUE)]
  ) %>%
    unique()

  data_variables_not_in_meta <- setdiff(
    setdiff(names(data), unique(c(present_ob_units, single_level_cols))),
    c(meta$col_name, meta$filter_grouping_column) %>% .[!is.na(.)]
  )

  number_of_variables_not_in_meta <- length(data_variables_not_in_meta)

  # add extra detail as to situations when it may be right to leave variables out of the metadata
  # add extra detail as to where to look (both in col_name and filter_grouping_column)

  if (number_of_variables_not_in_meta == 0) {
    output <- list(
      "message" = "All variables in the data file are observational units or are represented in the metadata file.",
      "result" = "PASS"
    )
  } else {
    if (number_of_variables_not_in_meta == 1) {
      output <- list(
        "message" = paste0("The following variable was found in the data file and isn't an observational unit, has only a single level, nor is represented in the metadata. <br> - Please check if this column is erroneous: '", paste0(data_variables_not_in_meta), "'."),
        "result" = "ADVISORY"
      )
    } else {
      output <- list(
        "message" = paste0("The following variables were found in the data file and aren't observational units, have only a single level, nor are represented in the metadata. <br> - Please check if these columns are erroneous: '", paste0(data_variables_not_in_meta, collapse = "', '"), "'."),
        "result" = "ADVISORY"
      )
    }
  }

  return(output)
}

# total -------------------------------------
# Check for Total in all filters

total <- function(data, meta) {
  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)

  filter_groups <- meta %>%
    filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  filters_and_groups <- c(filters, filter_groups)

  if (length(filters_and_groups) == 0) {
    output <- list(
      "message" = "There are no filters in the data file.",
      "result" = "IGNORE"
    )
  } else {
    dfilters <- select(data, all_of(filters_and_groups))

    total_check <- function(i) {
      if (!"Total" %in% dfilters[[i]]) {
        return("FAIL")
      } else {
        return("PASS")
      }
    }

    pre_result <- stack(sapply(filters_and_groups, total_check))

    if (all(pre_result$values == "PASS")) {
      output <- list(
        "message" = "All filters and groups have a 'Total' aggregation.",
        "result" = "PASS"
      )
    } else {
      missing_total <- filter(pre_result, values == "FAIL") %>% pull(ind)

      if (nrow(pre_result) == 1) {
        output <- list(
          "message" = paste0("A 'Total' aggregation should be added in '", paste(missing_total, collapse = "', '"), "' if applicable."),
          "result" = "ADVISORY"
        )
      } else {
        if (length(filters) > 0 && length(filter_groups) > 0) {
          output <- list(
            "message" = paste0("A 'Total' aggregation should be added in the following filters and groups if applicable: '", paste(missing_total, collapse = "', '"), "'."),
            "result" = "ADVISORY"
          )
        } else {
          if (length(filters) > 0) {
            output <- list(
              "message" = paste0("A 'Total' aggregation should be added in the following filters if applicable: '", paste(missing_total, collapse = "', '"), "'."),
              "result" = "ADVISORY"
            )
          } else {
            output <- list(
              "message" = paste0("A 'Total' aggregation should be added in the following filters groups if applicable: '", paste(missing_total, collapse = "', '"), "'."),
              "result" = "ADVISORY"
            )
          }
        }
      }
    }
  }

  return(output)
}

# observational_total -------------------------------------
# Check if Total has been used erroneously in any observational units

observational_total <- function(data, meta) {
  observational_total_check <- function(i) {
    if ("Total" %in% data[[i]] || "total" %in% data[[i]] || "all" %in% data[[i]] || "All" %in% data[[i]]) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  acceptable_ob_units_sch_prov_filter <- acceptable_observational_units[!acceptable_observational_units %in% c(geography_matrix[13, 3], geography_matrix[14, 3])]

  present_ob_units <- c(
    intersect(acceptable_ob_units_sch_prov_filter, names(data)),
    names(data)[grepl(potential_ob_units_regex, names(data), ignore.case = TRUE)]
  ) %>%
    unique()

  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)

  filter_groups <- meta %>%
    filter(!is.na(filter_grouping_column)) %>%
    filter(filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  if (
    length(filters) == 1 &&
      any(filters[1] %in% geography_matrix[13:14, 3], filter_groups[1] %in% geography_matrix[13:14, 3])
  ) {
    present_ob_units <- present_ob_units[!present_ob_units %in% c(geography_matrix[13, 3], geography_matrix[14, 3])]
  }

  pre_result <- stack(sapply(present_ob_units, observational_total_check))

  ob_units_with_total <- filter(pre_result, values == "FAIL") %>% pull(ind)

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "message" = "There are no Total or All values in the observational unit columns.",
      "result" = "PASS"
    )
  } else {
    ob_units_with_total <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(ob_units_with_total) == 1) {
      output <- list(
        "message" = paste0("There are Total or All rows in the following observational unit column: '", paste(ob_units_with_total, collapse = "', '"), "'. <br> - These cells should be blank."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("There are Total or All rows in the following observational unit columns: '", paste(ob_units_with_total, collapse = "', '"), "'. <br> - These cells should be blank."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# null -------------------------------------
# check for any NULL/Null/null values

null <- function(data, meta) {
  null_symbols <- c("NA", "na", "Null", "null", "NULL")

  pre_result <- as_tibble(cbind(
    "symbol" = null_symbols,
    "data" = null_symbols %in% unlist(data, use.names = FALSE),
    "meta" = null_symbols %in% unlist(meta, use.names = FALSE)
  ))

  if ((TRUE %in% pre_result$data) && (TRUE %in% pre_result$meta)) {
    output <- list(
      "message" = paste0(
        "The following problematic symbols were found in the data file: '", paste(pre_result %>% filter(data == TRUE) %>% pull(symbol), collapse = "', '"),
        "'. <br> The following problematic symbols were found in the metadata file: '", paste(pre_result %>% filter(meta == TRUE) %>% pull(symbol), collapse = "', '"),
        "'. <br> - Please refer to the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance document</a>",
        " if you are unsure of how to represent missing data, or ", "<a href='mailto: explore.statistics@education.gov.uk'>contact us</a>", " for advice."
      ),
      "result" = "FAIL"
    )
  } else {
    if (TRUE %in% pre_result$data) {
      output <- list(
        "message" = paste0(
          "The following problematic symbols were found in the data file: '", paste(pre_result %>% filter(data == TRUE) %>% pull(symbol), collapse = "', '"),
          "'. <br> - Please refer to the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance document</a>",
          " if you are unsure of how to represent missing data."
        ),
        "result" = "FAIL"
      )
    } else {
      if (TRUE %in% pre_result$meta) {
        output <- list(
          "message" = paste0(
            "The following problematic symbols were found in the metadata file: '", paste(pre_result %>% filter(meta == TRUE) %>% pull(symbol), collapse = "', '"),
            "', please remove these from the file. <br> - If you are unsure on how or what to replace them with, please ",
            "<a href='mailto: explore.statistics@education.gov.uk'>contact us</a>", " for advice."
          ),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = "No problematic NULL or NA values were found in the data or metadata files.",
          "result" = "PASS"
        )
      }
    }
  }

  return(output)
}

# suppression_symbols -------------------------------------
# check for the legacy suppression symbol 'c'

suppression_symbols <- function(data, meta) {
  mindicators <- filter(meta, col_type == "Indicator")

  present_indicators <- intersect(mindicators$col_name, names(data))

  suppression_symbols_check <- function(i) {
    if ("x" %in% data[[i]]) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  pre_result <- stack(sapply(present_indicators, suppression_symbols_check))

  if ("FAIL" %in% pre_result$values) {
    output <- list(
      "message" = "'x' was found in the indicator values, please update these to the GSS recommended 'c' where this refers to suppressed data.",
      "result" = "ADVISORY"
    )
  } else {
    output <- list(
      "message" = "The legacy symbol for suppression, 'x', is not present in the indicator values.",
      "result" = "PASS"
    )
  }

  return(output)
}

# no_data_symbols -------------------------------------
# check for legacy symbols for missing data

no_data_symbols <- function(data) {
  old_no_data_symbols <- c("N/A", "n/a", ".", "..", "-")

  pre_result <- as_tibble(cbind("symbol" = old_no_data_symbols, "found" = old_no_data_symbols %in% unlist(data, use.names = FALSE)))

  if (all(pre_result$found == FALSE)) {
    output <- list(
      "message" = "No legacy symbols for no data were found in the data file.",
      "result" = "PASS"
    )
  } else {
    present_legacy_symbols <- pre_result %>%
      filter(found == TRUE) %>%
      pull(symbol)

    if (length(present_legacy_symbols == 1)) {
      output <- list(
        "message" = paste0("The following legacy symbol was found in the data: '", paste0(present_legacy_symbols, collapse = "', '"), "'. <br> - Please check the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance</a>", " for advice on the symbols to use for no data."),
        "result" = "ADVISORY"
      )
    } else {
      output <- list(
        "message" = paste0("The following legacy symbols have been found in the data: '", paste0(present_legacy_symbols, collapse = "', '"), "'. <br> - Please check the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance</a>", " for advice on the symbols to use for no data."),
        "result" = "ADVISORY"
      )
    }
  }

  return(output)
}

# blanks_filters -------------------------------------
# check for blank cells in filters and filter groups

blanks_filters <- function(data, meta) {
  if (meta %>% filter(col_type == "Filter") %>% nrow() == 0) {
    output <- list(
      "message" = "There are no filters in the data file.",
      "result" = "IGNORE"
    )
  } else {
    blanks_check <- function(i) {
      if ("" %in% data[[i]]) {
        return("FAIL")
      } else {
        return("PASS")
      }
    }

    filters_groups <- c(
      meta %>% filter(col_type == "Filter") %>% pull(col_name),
      meta %>% filter(col_type == "Filter") %>% pull(filter_grouping_column)
    ) %>%
      unique() %>%
      .[. != ""]

    pre_result <- stack(sapply(filters_groups, blanks_check))

    filters_with_blanks <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (all(pre_result$values == "PASS")) {
      output <- list(
        "message" = "There are no blank values in any filters or filter groups.",
        "result" = "PASS"
      )
    } else {
      if (length(filters_with_blanks) == 1) {
        output <- list(
          "message" = paste0("There are blanks in the following filter or filter group: '", paste(filters_with_blanks, collapse = "', '"), "'. <br> - These cells must have a value. If they represent no specific breakdown, such as 'all genders' then you should use 'Total'."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("There are blanks in the following filters or filter groups: '", paste(filters_with_blanks, collapse = "', '"), "'. <br> - These cells must have a value. If they represent no specific breakdown, such as 'all genders' then you should use 'Total'."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# blanks_indicators -------------------------------------
# check for blank cells in filters and filter groups

blanks_indicators <- function(data, meta) {
  blanks_check <- function(i) {
    if ("" %in% data[[i]]) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }

  indicators <- meta %>%
    filter(col_type == "Indicator") %>%
    pull(col_name) %>%
    as.vector()

  pre_result <- stack(sapply(indicators, blanks_check))

  indicators_with_blanks <- filter(pre_result, values == "FAIL") %>% pull(ind)

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "message" = "There are no blank values in any indicators.",
      "result" = "PASS"
    )
  } else {
    if (length(indicators_with_blanks) == 1) {
      output <- list(
        "message" = paste0("There are blanks in the following indicator: '", paste(indicators_with_blanks, collapse = "', '"), "'. <br> - Blank cells are problematic and must be avoided. <br> - Please check the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance</a>", " for advice on the symbols to use for no data."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("There are blanks in the following indicators: '", paste(indicators_with_blanks, collapse = "', '"), "'. <br> - Blank cells are problematic and must be avoided. <br> - Please check the ", "<a href='https://gss.civilservice.gov.uk/wp-content/uploads/2017/03/GSS-Website-Harmonised-Symbols-Supporting-Documentation.pdf' target='_blank'>GSS guidance</a>", " for advice on the symbols to use for no data."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# time_period -------------------------------------
# checking that if the time_identifier is X, then the time_period is Y

time_period <- function(data) {
  base_identifier <- data$time_identifier[1]
  time_length <- data
  time_length[["digits"]] <- str_count(time_length[["time_period"]])

  if (base_identifier %in% four_digit_identifiers) {
    if ((nrow(filter(time_length, digits == 4)) == nrow(time_length)) == FALSE) {
      output <- list(
        "message" = paste0("The time_period length for '", paste(base_identifier), "' must always be a four digit number. <br> - Please check the ", "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values' target='_blank'>guidance website</a> if you are unsure."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "The time_period length matches the time_identifier values in the data file.",
        "result" = "PASS"
      )
    }
  }

  if (base_identifier %in% six_digit_identifiers) {
    if ((nrow(filter(time_length, digits == 6)) == nrow(time_length)) == FALSE) {
      output <- list(
        "message" = paste0("The time_period length for '", paste(base_identifier), "' must always be a six digit number. <br> - Please check the ", "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#list_of_allowable_time_values' target='_blank'>guidance website</a> if you are unsure."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "The time_period length matches the time_identifier values in the data file.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# time_period_six -------------------------------------
# then if 6 digit if it shows consecutive years

time_period_six <- function(data) {
  time_length <- data
  time_length$digits <- str_count(time_length$time_period)
  six_digit_years <- filter(time_length, digits == 6)

  time_period_six_check <- function(i) {
    currentyearend <- as.numeric(substr(i, 3, 4))
    nextyearend <- as.numeric(substr(i, 5, 6))

    if (currentyearend == 99 && nextyearend == 0) {
      return("PASS")
    } else {
      if ((currentyearend + 1) == nextyearend) {
        return("PASS")
      } else {
        return("FAIL")
      }
    }
  }

  pre_result <- sapply(unique(six_digit_years$time_period), time_period_six_check)

  if (nrow(filter(time_length, digits == 6)) == 0) {
    output <- list(
      "message" = "There are no six digit time_period values in the file.",
      "result" = "IGNORE"
    )
  } else {
    if ("FAIL" %in% pre_result) {
      output <- list(
        "message" = "When the time period is six digits, the years must be consecutive such as 201920.",
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "The six digit time_period values refer to consecutive years.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# three_years -------------------------------------
# produce a warning if there are fewer than 3 years of data in the file

three_years <- function(data) {
  if (length(unique(data$time_period)) < 3) {
    output <- list(
      "message" = "The data file contains fewer than three different years of data. <br> - Where it exists, you should include at least 3 years of data in your file to meet upcoming changes in accessibility legislation.",
      "result" = "ADVISORY"
    )
  } else {
    output <- list(
      "message" = "The data file contains at least three different years of data.",
      "result" = "PASS"
    )
  }

  return(output)
}

# region_for_la -------------------------------------
# check if there is LA level data, and if so, if regional columns are present and completed

region_for_la <- function(data) {
  if (!geography_matrix[3, 1] %in% unique(data$geographic_level)) {
    output <- list(
      "message" = paste("There is no", geography_matrix[3, 1], "level data in the data file."),
      "result" = "IGNORE"
    )
  } else {

    # not testing for individual columns as region_col_completed covers that

    if (!(geography_matrix[2, 2] %in% names(data)) | !(geography_matrix[2, 3] %in% names(data))) {
      output <- list(
        "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "are missing from the data file. <br> -", geography_matrix[2, 1], "information should ideally be given for all", geography_matrix[3, 1], "level data."),
        "result" = "ADVISORY"
      )
    } else {
      region_cols <- data %>%
        filter(geographic_level == geography_matrix[3, 1]) %>%
        select(geography_matrix[2, 2], geography_matrix[2, 3])

      missing_region_codes <- sum(is.na(select(region_cols, geography_matrix[2, 2])))
      missing_region_names <- sum(is.na(select(region_cols, geography_matrix[2, 3])))

      if (missing_region_codes > 0 && missing_region_names > 0) {
        output <- list(
          "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "have missing values for", geography_matrix[3, 1], "rows in the data file. <br> - It is recommended to include the information from these columns for", geography_matrix[3, 1], "level data."),
          "result" = "ADVISORY"
        )
      } else {
        output <- list(
          "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "are completed for all", geography_matrix[3, 1], "rows in the data file."),
          "result" = "PASS"
        )
      }
    }
  }

  return(output)
}

# region_for_lad -------------------------------------
# check if there is LA level data, and if so, if regional columns are present and completed

region_for_lad <- function(data) {
  if (!geography_matrix[4, 1] %in% unique(data$geographic_level)) {
    output <- list(
      "message" = paste("There is no", geography_matrix[4, 1], "level data in the data file."),
      "result" = "IGNORE"
    )
  } else {

    # not testing for individual columns as region_col_completed covers that

    if (!(geography_matrix[2, 2] %in% names(data)) | !(geography_matrix[2, 3] %in% names(data))) {
      output <- list(
        "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "are missing from the data file. <br> -", geography_matrix[2, 1], "information should ideally be given for all", geography_matrix[4, 1], "level data."),
        "result" = "ADVISORY"
      )
    } else {
      region_cols <- data %>%
        filter(geographic_level == geography_matrix[4, 1]) %>%
        select(region_code, region_name)

      missing_region_codes <- sum(is.na(select(region_cols, region_code)))
      missing_region_names <- sum(is.na(select(region_cols, region_name)))

      if (missing_region_codes > 0 && missing_region_names > 0) {
        output <- list(
          "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "have missing values for", geography_matrix[4, 1], "rows in the data file. <br> - It is recommended to include the information from these columns for", geography_matrix[4, 1], "level data."),
          "result" = "ADVISORY"
        )
      } else {
        output <- list(
          "message" = paste("Both", geography_matrix[2, 2], "and", geography_matrix[2, 3], "are completed for all", geography_matrix[4, 1], "rows in the data file."),
          "result" = "PASS"
        )
      }
    }
  }

  return(output)
}

# geography_level_completed -------------------------------------
# Are the geography levels completed as expected

geography_level_completed <- function(data) {
  incomplete_cols <- function(i) {
    # if a geographic level is present, then this returns incomplete cols for those rows from the pre-defined geography_matrix

    if (i[1] %in% data$geographic_level) {
      level_rows <- data %>% filter(geographic_level == i[1])

      cols <- i[2:4] %>% .[!is.na(.)]

      col_completed <- function(x) {
        y <- x + 1
        col <- paste(i[y])

        if (any(is.na(level_rows[[col]]))) {
          return(col)
        }
      }

      pre_output <- sapply(c(1:length(cols)), col_completed)

      return(pre_output)
    }
  }

  # filter out the non table tool rows / cols from geography matrix (also removed new_la_code as that can legitimately be blank)
  geography_completed <- geography_matrix[1:14, 1:3]

  incomplete_geographies <- unlist(apply(geography_completed, 1, incomplete_cols))

  if (length(incomplete_geographies) == 0) {
    output <- list(
      "message" = "All geographic columns are completed as expected.",
      "result" = "PASS"
    )
  } else {
    if (length(incomplete_geographies) == 1) {
      output <- list(
        "message" = paste0("The '", paste(incomplete_geographies), "' column should be completed for all '", paste(col_to_level_lookup %>% filter(cols == incomplete_geographies) %>% pull(levels)), "' rows."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0(
          "The following columns should be completed for all rows of the associated level that they refer to: <br> - '", paste0(incomplete_geographies, collapse = "', '"), "' . <br> - If you are unsure of the levels that they refer to, please check the ",
          "<a href='https://rsconnect/rsc/stats-production-guidance/ud.html#allowable_geographic_levels' target='_blank'>allowable geographic values table</a>."
        ),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# region_col_completed -------------------------------------
# When one of region name and code is completed, is the other also?

region_col_completed <- function(data) {
  if ((geography_matrix[2, 2] %in% names(data)) && (geography_matrix[2, 3] %in% names(data))) {
    region_both_complete_check <- function(data) {
      if (is.na(data[[geography_matrix[2, 2]]]) && !is.na(data[[geography_matrix[2, 3]]])) {
        return("code_missing")
      } else {
        if (is.na(data[[geography_matrix[2, 3]]]) && !is.na(data[[geography_matrix[2, 2]]])) {
          return("name_missing")
        }
      }
    }

    pre_result <- apply(data, 1, region_both_complete_check)

    if (is.null(pre_result)) {
      output <- list(
        "message" = paste("Where one of", geography_matrix[2, 2], "or", geography_matrix[2, 3], "is completed, the other column is also completed."),
        "result" = "PASS"
      )
    } else {
      if (all(c("code_missing", "name_missing") %in% pre_result)) {
        output <- list(
          "message" = paste("Where one of", geography_matrix[2, 2], "or", geography_matrix[2, 3], "is completed, the other column should also be completed."),
          "result" = "FAIL"
        )
      } else {
        if ("code_missing" %in% pre_result) {
          output <- list(
            "message" = paste("Where", geography_matrix[2, 3], "is completed,", geography_matrix[2, 2], "should also be completed."),
            "result" = "FAIL"
          )
        } else {
          output <- list(
            "message" = paste("Where", geography_matrix[2, 2], "is completed,", geography_matrix[2, 3], "should also be completed."),
            "result" = "FAIL"
          )
        }
      }
    }
  } else {
    if (geography_matrix[2, 3] %in% names(data)) {
      output <- list(
        "message" = paste("Where", geography_matrix[2, 3], "is included in the data file,", geography_matrix[2, 2], "should also be included"),
        "result" = "FAIL"
      )
    } else {
      if (geography_matrix[2, 2] %in% names(data)) {
        output <- list(
          "message" = paste("Where", geography_matrix[2, 2], "is included in the data file,", geography_matrix[2, 3], "should also be included"),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste("No recognised", geography_matrix[2, 1], "columns are present in this data file."),
          "result" = "IGNORE"
        )
      }
    }
  }

  return(output)
}

# overcompleted_cols -------------------------------------
# Are any columns completed for unexpected rows

overcompleted_cols <- function(data, meta) {

  # ----------------------------------------------------------------------------------------------------------------------------------
  # checking if region cols are completed in national rows

  overcomplete_regional_cols <- function(matrixRow) {

    # Start by filtering the data down to remove the geographic level being tested and any lower levels we don't care about

    level_rows <- data %>%
      filter(geographic_level != matrixRow[1]) %>%
      filter(!geographic_level %in% geography_matrix[3:16, ])

    # Extract the columns for the geographic level that is being tested

    cols <- matrixRow[2:4] %>% .[!is.na(.)]

    # Function used to check if each column for that geographic level has any cells that are not blank

    col_completed <- function(x) {
      y <- x + 1
      col <- paste(matrixRow[y])

      if (any(!is.na(level_rows[[col]] %>% .[. != ""]))) {
        return(col)
      }
    }

    # Apply over every column in the matrixRow (geographic_level) being tested

    pre_output <- sapply(c(1:length(cols)), col_completed)

    return(pre_output)
  }

  # ----------------------------------------------------------------------------------------------------------------------------------
  # checking if local authority columns are completed for national, regional or mid-geography rows (ignoring LAD)

  overcomplete_la_cols <- function(matrixRow) {

    # This is a test that could benefit from more detail, and maybe a table in the error feedback

    # Start by filtering the data down to remove the geographic level being tested, lad rows and any lower levels we don't care about

    level_rows <- data %>%
      filter(geographic_level != matrixRow[1]) %>%
      filter(!geographic_level %in% geography_matrix[13:16, ]) %>%
      filter(geographic_level != geography_matrix[4, 1])

    # Extract the columns for the geographic level that is being tested

    cols <- matrixRow[2:4] %>% .[!is.na(.)]

    # Function used to check if each column for that geographic level has any cells that are not blank

    col_completed <- function(x) {
      y <- x + 1
      col <- paste(matrixRow[y])

      if (any(!is.na(level_rows[[col]] %>% .[. != ""]))) {
        return(col)
      }
    }

    # Apply over every column in the matrixRow (geographic_level) being tested

    pre_output <- sapply(c(1:length(cols)), col_completed)

    return(pre_output)
  }

  # ----------------------------------------------------------------------------------------------------------------------------------
  # checking if mid-geography cols are completed for unexpected levels

  overcomplete_mid_cols <- function(matrixRow) {

    # Start by filtering the data down to remove the geographic level being tested and any lower levels we don't care about

    level_rows <- data %>%
      filter(geographic_level != matrixRow[1]) %>%
      filter(!geographic_level %in% geography_matrix[13:16, ])

    # Extract the columns for the geographic level that is being tested

    cols <- matrixRow[2:4] %>% .[!is.na(.)]

    # Function used to check if each column for that geographic level has any cells that are not blank

    col_completed <- function(x) {
      y <- x + 1
      col <- paste(matrixRow[y])

      if (any(!is.na(level_rows[[col]] %>% .[. != ""]))) {
        return(col)
      }
    }

    # Apply over every column in the matrixRow (geographic_level) being tested

    pre_output <- sapply(c(1:length(cols)), col_completed)

    return(pre_output)
  }

  # ----------------------------------------------------------------------------------------------------------------------------------
  # checking if low level geographies are completed for any rows other than their own

  overcomplete_low_cols <- function(matrixRow) {

    # Filtering the data down to remove the geographic level being tested and any lower levels we don't care about

    level_rows <- data %>% filter(geographic_level != matrixRow[1])

    # Extract the columns for the geographic level that is being tested

    cols <- matrixRow[2:4] %>% .[!is.na(.)]

    # Function used to check if each column for that geographic level has any cells that are not blank

    col_completed <- function(x) {
      y <- x + 1
      col <- paste(matrixRow[y])

      if (any(!is.na(level_rows[[col]] %>% .[. != ""]))) {
        return(col)
      }
    }

    # flagging if sch or prov level and name is only filter
    filters <- meta %>%
      filter(col_type == "Filter") %>%
      pull(col_name)

    filter_groups <- meta %>%
      filter(!is.na(filter_grouping_column)) %>%
      filter(filter_grouping_column != "") %>%
      pull(filter_grouping_column)

    if (
      matrixRow[3] %in% geography_matrix[13:14, 3] &&
        length(filters) == 1 &&
        any(filters[1] %in% geography_matrix[13:14, 3], filter_groups[1] %in% geography_matrix[13:14, 3])
    ) {
      sch_prov_only_filter <- TRUE
    } else {
      sch_prov_only_filter <- FALSE
    }

    # Apply over every column in the matrixRow (geographic_level) being tested

    if (sch_prov_only_filter == TRUE) {
      pre_output <- sapply(c(1, 3), col_completed)
    } else {
      pre_output <- sapply(c(1:length(cols)), col_completed)
    }

    return(pre_output)
  }

  # ----------------------------------------------------------------------------------------------------------------------------------
  # forcing these into a matrix, otherwise just calling that row returns a vector that breaks the apply function

  regional_matrix <- matrix(geography_matrix[2, ], nrow = 1)
  la_matrix <- matrix(geography_matrix[3, ], nrow = 1)

  overcomplete_geographies <- c(
    unlist(apply(regional_matrix, 1, overcomplete_regional_cols)),
    unlist(apply(la_matrix, 1, overcomplete_la_cols)),
    unlist(apply(geography_matrix[4:12, ], 1, overcomplete_mid_cols)),
    unlist(apply(geography_matrix[13:16, ], 1, overcomplete_low_cols))
  )

  if (length(overcomplete_geographies) == 0) {
    output <- list(
      "message" = "All geographic columns are empty where expected.",
      "result" = "PASS"
    )
  } else {
    if (length(overcomplete_geographies) == 1) {
      output <- list(
        "message" = paste0("The '", paste(overcomplete_geographies), "' column is completed for unexpected geographic_level rows. <br> - Please ", "<a href='mailto: explore.statistics@education.gov.uk'>contact us</a>", " if you are unsure of how to fix this."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following columns are completed for unexpected geographic_level rows: '", paste0(overcomplete_geographies, collapse = "', '"), "'. <br> - Please ", "<a href='mailto: explore.statistics@education.gov.uk'>contact us</a>", " if you are unsure of how to fix this."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# ignored_rows -------------------------------------
# What rows will be ignored by the table tool

ignored_rows <- function(data) {
  table_tool_rows <- data %>%
    filter(geographic_level != geography_matrix[15, 1]) %>%
    filter(geographic_level != geography_matrix[16, 1]) %>%
    nrow()

  if (table_tool_rows == 0) {
    output <- list(
      "message" = "This file only contains rows ignored by the EES table tool, and should be uploaded to the release without the metadata file as an ancillary file.",
      "result" = "ANCILLARY"
    )
  } else {
    potential_ignored_rows <- data %>%
      filter(geographic_level %in% geography_matrix[13:16, 1]) %>%
      nrow()

    if (potential_ignored_rows == 0) {
      output <- list(
        "message" = "No rows in the file will be ignored by the EES table tool.",
        "result" = "PASS"
      )
    } else {
      levels_present <- data %>%
        distinct(geographic_level)

      if (nrow(levels_present) == 1 && data$geographic_level[1] %in% geography_matrix[13:14, 1]) {
        output <- list(
          "message" = "No rows in the file will be ignored by the EES table tool.",
          "result" = "PASS"
        )
      } else {
        if (geography_matrix[13, 1] %in% levels_present$geographic_level && geography_matrix[14, 1] %in% levels_present$geographic_level) {
          output <- list(
            "message" = paste(geography_matrix[13, 1], "and", geography_matrix[14, 1], "data has been mixed - please contact the Statistics Development Team."),
            "result" = "FAIL"
          )
        } else {
          output <- list(
            "message" = paste0(
              potential_ignored_rows, " rows of data will be ignored by the table tool. <br> - These will be at ", geography_matrix[13, 1], ", ", geography_matrix[14, 1], ", ", geography_matrix[15, 1], " and ", geography_matrix[16, 1], " level. <br> - Please ",
              "<a href='mailto: explore.statistics@education.gov.uk'>contact us</a>", " or see our ",
              "<a href='https://dfe-analytical-services.github.io/stats-production-guidance-copy/ud.html#Allowable_geographic_levels' target='_blank'>guidance website</a>", # a message that we should add the option to see those rows in another tab at some point
              " for more information."
            ),
            "result" = "PASS WITH NOTE"
          )
        }
      }
    }
  }
  return(output)
}

# la_combinations -------------------------------------
# checking that la code and name combinations are valid

la_combinations <- function(data) {
  if (!"new_la_code" %in% names(data)) {
    output <- list(
      "message" = "LA columns are not present in this data file.",
      "result" = "IGNORE"
    )
  } else {
    invalid_values <- data %>%
      select("old_la_code", "new_la_code", "la_name") %>%
      unique() %>%
      filter(!is.na(.)) %>%
      filter(new_la_code != "") %>%
      filter(new_la_code != ":") %>%
      filter(new_la_code != "z") %>%
      mutate(combo = paste(old_la_code, new_la_code, la_name)) %>%
      pull(combo) %>%
      .[!(. %in% expected_la_combinations)]

    if (length(invalid_values) == 0) {
      output <- list(
        "message" = "All old_la_code, new_la_code and la_name comninations are valid.",
        "result" = "PASS"
      )
    } else {
      if (length(invalid_values) == 1) {
        output <- list(
          "message" = paste0("The following old_la_code, new_la_code and la_name combination is invalid: '", paste0(invalid_values), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/las.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating local authority level data."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The following old_la_code, new_la_code and la_name combinations are invalid: '", paste0(invalid_values, collapse = "', '"), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/las.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating local authority level data."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# region_combinations -------------------------------------
# Checking that region_code and region_name combinations are valid
## Need to update reference list in error message to whatever method we use for LAs as the portal list doesn't include inner/outer london (which we allow)

region_combinations <- function(data) {
  if (!geography_matrix[2, 2] %in% names(data)) {
    output <- list(
      "message" =  paste(geography_matrix[2, 2], "columns are not present in this data file."),
      "result" = "IGNORE"
    )
  } else {
    invalid_values <- data %>%
      select(geography_matrix[2, 2], geography_matrix[2, 3]) %>%
      unique() %>%
      filter(!is.na(.)) %>%
      filter(region_code != "") %>%
      filter(region_code != ":") %>%
      filter(region_code != "z") %>%
      mutate(combo = paste(region_code, region_name)) %>%
      pull(combo) %>%
      .[!(. %in% expected_region_combinations)]

    if (length(invalid_values) == 0) {
      output <- list(
        "message" = "All region_code and region_name combinations are valid.",
        "result" = "PASS"
      )
    } else {
      if (length(invalid_values) == 1) {
        output <- list(
          "message" = paste0("The following region_code and region_name combination is invalid: '", paste0(invalid_values), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/regions.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating region level data."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The following region_code / region_name combinations are invalid: '", paste0(invalid_values, collapse = "', '"), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/regions.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating region level data."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# country_combinations -------------------------------------
# checking that country_code and country_name combinations are valid

country_combinations <- function(data) {
  if (!"country_code" %in% names(data)) {
    output <- list(
      "message" = "Country columns are not present in this data file.",
      "result" = "IGNORE"
    )
  } else {
    invalid_values <- data %>%
      select("country_code", "country_name") %>%
      filter(country_code != ":") %>%
      filter(country_code != "z") %>%
      unique() %>%
      mutate(combo = paste(country_code, country_name)) %>%
      pull(combo) %>%
      .[!(. %in% expected_country_combinations)]

    if (length(invalid_values) == 0) {
      output <- list(
        "message" = "All country_code and country_name combinations are valid.",
        "result" = "PASS"
      )
    } else {
      if (length(invalid_values) == 1) {
        output <- list(
          "message" = paste0("The following country_code / country_name combination is invalid: '", paste0(invalid_values), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/country.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating country level data."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The following country_code / country_name combinations are invalid: '", paste0(invalid_values, collapse = "', '"), "'. <br> - We do not expect any combinations outside of the <a href='https://github.com/dfe-analytical-services/dfe-published-data-qa/blob/master/data/country.csv' target='_blank'>standard geographies</a> (case sensitive), you can use our [guidance link TBC] for help creating country level data."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# school_laestab_duplicates  ----------------------------------------
# check that there is a 1:1 relationship between school laestab codes and names

school_laestab_duplicates <- function(data) {
  if (!geography_matrix[13, 1] %in% unique(data$geographic_level)) {
    output <- list(
      "message" = paste0(geography_matrix[13, 1], "-level data is not present in this data file."),
      "result" = "IGNORE"
    )
  } else {
    multi_count_code <- data %>%
      select(geography_matrix[13, 4], geography_matrix[13, 3]) %>%
      distinct() %>%
      add_count(school_name, name = "school_name_n") %>%
      filter(school_name_n > 1) %>%
      mutate(combo = paste0(school_name, " - has ", school_name_n, " different laestabs")) %>%
      select(combo) %>%
      distinct() %>%
      pull()

    multi_count_name <- data %>%
      select(geography_matrix[13, 4], geography_matrix[13, 3]) %>%
      distinct() %>%
      add_count(school_laestab, name = "school_code_n") %>%
      filter(school_code_n > 1) %>%
      mutate(combo = paste(school_laestab, " - has ", school_code_n, " different school names")) %>%
      filter(school_code_n > 1) %>%
      select(combo) %>%
      distinct() %>%
      pull()


    if (length(multi_count_code) == 0 & length(multi_count_name) == 0) {
      output <- list(
        "message" = paste("Every", geography_matrix[13, 3], "value has one", geography_matrix[13, 4], "value, and vice versa."),
        "result" = "PASS"
      )
    } else {
      if (length(multi_count_code) == 1 & length(multi_count_name) == 0) {
        output <- list(
          "message" = paste0("The following ", geography_matrix[13, 3], "value has multuple", geography_matrix[13, 4], " values: ", paste0(multi_count_code), ". 
                             <br> - Every school name should have only one laestab and vice versa."),
          "result" = "FAIL"
        )
      } else {
        if (length(multi_count_code) == 1 & length(multi_count_name) == 1) {
          output <- list(
            "message" = paste0("The following school_name has multiple laestabs: ", paste0(multi_count_code), ", 
                              <br> and the following laestab has multiple names: ", paste0(multi_count_name), ".
                             <br> - Every school name should have only one laestab and vice versa."),
            "result" = "FAIL"
          )
        } else {
          if (length(multi_count_code) > 1 & length(multi_count_name) == 1) {
            output <- list(
              "message" = paste0("The following school_names have multiple laestabs: ", paste0(multi_count_code, collapse = ", "), ", 
                             <br> and the following laestab has multiple names: ", paste0(multi_count_name), ".
                             <br> - Every school name should have only one laestab and vice versa."),
              "result" = "FAIL"
            )
          } else {
            if (length(multi_count_code) == 1 & length(multi_count_name) > 1) {
              output <- list(
                "message" = paste0("The following school_name has multiple laestabs: ", paste0(multi_count_code), ", 
                              <br> and the following laestabs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one laestab and vice versa."),
                "result" = "FAIL"
              )
            } else {
              if (length(multi_count_code) == 0 & length(multi_count_name) > 1) {
                output <- list(
                  "message" = paste0("The following laestabs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one laestab and vice versa."),
                  "result" = "FAIL"
                )
              } else {
                output <- list(
                  "message" = paste0("The following school_names have multiple laestabs: ", paste0(multi_count_code, collapse = ", "), ", 
                             <br> and the following laestabs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one laestab and vice versa."),
                  "result" = "FAIL"
                )
              }
            }
          }
        }
      }
    }
  }
  return(output)
}

# school_urn_duplicates  ----------------------------------------
# check that there is a 1:1 relationship between school urns and names

school_urn_duplicates <- function(data) {
  if (!"School" %in% unique(data$geographic_level)) {
    output <- list(
      "message" = "School-level data is not present in this data file.",
      "result" = "IGNORE"
    )
  } else {
    multi_count_code <- data %>%
      select("school_urn", "school_name") %>%
      distinct() %>%
      add_count(school_name, name = "school_name_n") %>%
      filter(school_name_n > 1) %>%
      mutate(combo = paste0(school_name, " - has ", school_name_n, " different URNs")) %>%
      select(combo) %>%
      distinct() %>%
      pull()

    multi_count_name <- data %>%
      select("school_urn", "school_name") %>%
      distinct() %>%
      add_count(school_urn, name = "school_code_n") %>%
      filter(school_code_n > 1) %>%
      mutate(combo = paste(school_urn, " - has ", school_code_n, " different school names")) %>%
      filter(school_code_n > 1) %>%
      select(combo) %>%
      distinct() %>%
      pull()


    if (length(multi_count_code) == 0 & length(multi_count_name) == 0) {
      output <- list(
        "message" = "Every school name has one URN, and vice versa.",
        "result" = "PASS"
      )
    } else {
      if (length(multi_count_code) == 1 & length(multi_count_name) == 0) {
        output <- list(
          "message" = paste0("The following school_name has multiple URNs: ", paste0(multi_count_code), ". 
                             <br> - Every school name should have only one URN and vice versa."),
          "result" = "FAIL"
        )
      } else {
        if (length(multi_count_code) == 1 & length(multi_count_name) == 1) {
          output <- list(
            "message" = paste0("The following school_name has multiple URNs: ", paste0(multi_count_code), ", 
                              <br> and the following URN has multiple names: ", paste0(multi_count_name), ".
                             <br> - Every school name should have only one URN and vice versa."),
            "result" = "FAIL"
          )
        } else {
          if (length(multi_count_code) > 1 & length(multi_count_name) == 1) {
            output <- list(
              "message" = paste0("The following school_names have multiple URNs: ", paste0(multi_count_code, collapse = ", "), ", 
                              <br> and the following URN has multiple names: ", paste0(multi_count_name), ".
                             <br> - Every school name should have only one URN and vice versa."),
              "result" = "FAIL"
            )
          } else {
            if (length(multi_count_code) == 1 & length(multi_count_name) > 1) {
              output <- list(
                "message" = paste0("The following school_name has multiple URNs: ", paste0(multi_count_code), ", 
                              <br> and the following URNs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one URN and vice versa."),
                "result" = "FAIL"
              )
            } else {
              if (length(multi_count_code) == 0 & length(multi_count_name) > 1) {
                output <- list(
                  "message" = paste0("The following URNs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one URN and vice versa."),
                  "result" = "FAIL"
                )
              } else {
                output <- list(
                  "message" = paste0("The following school_names have multiple codes: ", paste0(multi_count_code, collapse = ", "), ", 
                              <br> and the following URNs have multiple names: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Every school name should have only one URN and vice versa."),
                  "result" = "FAIL"
                )
              }
            }
          }
        }
      }
    }
  }
  return(output)
}


# other_geography_duplicates  ----------------------------------------
# check that there is a 1:1 relationship between geography codes and names

lower_level_geog_names <- geography_matrix[c(7:12, 14), 2:3] %>% as.character() # skipping school as it has it's own tests

other_geography_duplicates <- function(data) {
  if (!any(lower_level_geog_names %in% names(data))) {
    output <- list(
      "message" = "Lower-level geography data is not present in this data file.",
      "result" = "IGNORE"
    )
  } else {
    geog_data <- data %>%
      select(any_of(c(
        "geographic_level", lower_level_geog_names
      ))) %>%
      distinct() %>%
      mutate(ID = 1:n())

    names <- geog_data %>%
      select(ID, geographic_level, contains("name")) %>%
      mutate_if(is.numeric, as.character) %>%
      melt(id.vars = c("ID", "geographic_level"), na.rm = TRUE) %>%
      select(ID, geographic_level, name = value)

    codes <- geog_data %>%
      select(ID, geographic_level, !contains("name")) %>%
      mutate_if(is.numeric, as.character) %>%
      melt(id.vars = c("ID", "geographic_level"), na.rm = TRUE) %>%
      select(ID, geographic_level, code = value)

    lookup_creator <- names %>%
      full_join(codes, by = c("ID", "geographic_level")) %>%
      select(-ID, -geographic_level) %>%
      distinct() %>%
      add_count(name, name = "name_n") %>%
      add_count(code, name = "code_n")

    multi_count_name <- lookup_creator %>%
      filter(name_n > 1) %>%
      mutate(combo = paste0(name, " - ", name_n, " different codes")) %>%
      select(combo) %>%
      distinct() %>%
      pull()

    if (length(multi_count_name) == 0) {
      output <- list(
        "message" = "Every geography has one code.",
        "result" = "PASS"
      )
    } else {
      if (length(multi_count_name) == 1) {
        output <- list(
          "message" = paste0("The following geography has multiple codes: ", paste0(multi_count_name), ".
                             <br> - Each geography should have only one code."),
          "result" = "FAIL"
        )
      } else {
        if (length(multi_count_name) > 1) {
          output <- list(
            "message" = paste0("The following geographies have multiple codes: ", paste0(multi_count_name, collapse = ", "), ".
                             <br> - Each geography should have only one code."),
            "result" = "FAIL"
          )
        }
      }
    }
  }
  return(output)
}

# other_geography_code_duplicates  ----------------------------------------
# check that there is a 1:1 relationship between geography names and codes

lower_level_geog_names <- geography_matrix[c(7:12, 14), 2:3] %>% as.character() # skipping school as it has it's own tests

other_geography_code_duplicates <- function(data) {
  if (!any(lower_level_geog_names %in% names(data))) {
    output <- list(
      "message" = "Lower-level geography data is not present in this data file.",
      "result" = "IGNORE"
    )
  } else {
    geog_data <- data %>%
      select(any_of(c(
        "geographic_level", lower_level_geog_names
      ))) %>%
      distinct() %>%
      mutate(ID = 1:n())

    names <- geog_data %>%
      select(ID, geographic_level, contains("name")) %>%
      mutate_if(is.numeric, as.character) %>%
      melt(id.vars = c("ID", "geographic_level"), na.rm = TRUE) %>%
      select(ID, geographic_level, name = value)

    codes <- geog_data %>%
      select(ID, geographic_level, !contains("name")) %>%
      mutate_if(is.numeric, as.character) %>%
      melt(id.vars = c("ID", "geographic_level"), na.rm = TRUE) %>%
      select(ID, geographic_level, code = value)

    lookup_creator <- names %>%
      full_join(codes, by = c("ID", "geographic_level")) %>%
      select(-ID, -geographic_level) %>%
      distinct() %>%
      add_count(name, name = "name_n") %>%
      add_count(code, name = "code_n")

    multi_count_code <- lookup_creator %>%
      filter(code_n > 1) %>%
      mutate(combo = paste0(code, " - ", code_n, " different names")) %>%
      select(combo) %>%
      distinct() %>%
      pull()

    if (length(multi_count_code) == 0) {
      output <- list(
        "message" = "Every geography code has one assigned geography.",
        "result" = "PASS"
      )
    } else {
      if (length(multi_count_code) == 1) {
        output <- list(
          "message" = paste0("The following geography code has multiple assigned geographies: ", paste0(multi_count_code), ". 
                             <br> - Each geography code should have only one assigned geography."),
          "result" = "FAIL"
        )
      } else {
        if (length(multi_count_code) > 1) {
          output <- list(
            "message" = paste0("The following geography codes have multiple assigned geographies: ", paste0(multi_count_code, collapse = ", "), ".
                             <br> - Each geography code should have only one assigned geography."),
            "result" = "FAIL"
          )
        }
      }
    }
  }
  return(output)
}

# na_geography -------------------------------------
# checking if location has code of ":", then name is "not available"

na_geography <- function(data) {
  geography_name_codes <- geography_matrix[1:14, 2:3] %>%
    as.character() %>%
    .[!is.na(.)]

  geog_data <- data %>%
    select(any_of(c(
      "geographic_level", geography_name_codes
    ))) %>%
    distinct()

  na_check <- function(level) {
    code_col <- geography_matrix[which(geography_matrix[, 1] == level), 2]
    name_col <- geography_matrix[which(geography_matrix[, 1] == level), 3]

    na_locations <- eval(parse(text = paste0(
      "geog_data %>% distinct(", code_col, ", ", name_col, ") %>% rename(code = ", code_col, ", name = ", name_col, ")"
    ))) %>% subset(code == ":" & !(name == "Not available"))

    if (nrow(na_locations) == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  testable_levels_present <- data %>%
    filter(geographic_level %in% c(geography_matrix[c(4, 6:14), 1])) %>% # Removing country, region, la, rsc region and planning area/institution
    distinct(geographic_level) %>%
    pull(geographic_level)

  if (length(testable_levels_present) == 0) {
    output <- list(
      "message" = paste0("No applicable locations to test."),
      "result" = "IGNORE"
    )
    return(output)
  }

  singleLevelTidy <- function(value, level) {
    if (value == FALSE) {
      return()
    } else {
      return(level)
    }
  }

  if (length(testable_levels_present) == 1) {
    na_names <- na_check(paste(testable_levels_present)) %>% singleLevelTidy(., testable_levels_present)
  } else {
    na_names <- stack(sapply(testable_levels_present, na_check)) %>%
      filter(values == TRUE) %>%
      pull(ind)
  }

  if (length(na_names) == 0) {
    output <- list(
      "message" = paste0("No tested locations have a code of ':' without the corresponding name 'Not available'."),
      "result" = "PASS"
    )
  } else {
    if (length(na_names) == 1) {
      output <- list(
        "message" = paste0("The following geographic level has at least one location with a code of ':', but does not have the corresponding name 'Not available': '", paste0(na_names), ". <br> - The name for ':' should always be 'Not available'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following geographic level has at least one location with a code of ':', but does not have the corresponding name 'Not available': '", paste0(na_names, collapse = "', '"), "'. <br> - The name for ':' should always be 'Not available'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# na_geography_code -------------------------------------
# checking if location has the name "not available" then its code is ":"

na_geography_code <- function(data) {
  geography_name_codes <- geography_matrix[1:14, 2:3] %>%
    as.character() %>%
    .[!is.na(.)]

  geog_data <- data %>%
    select(any_of(c(
      "geographic_level", geography_name_codes
    ))) %>%
    distinct()

  na_check <- function(level) {
    code_col <- geography_matrix[which(geography_matrix[, 1] == level), 2]
    name_col <- geography_matrix[which(geography_matrix[, 1] == level), 3]

    na_locations <- eval(parse(text = paste0(
      "geog_data %>% distinct(", code_col, ", ", name_col, ") %>% rename(code = ", code_col, ", name = ", name_col, ")"
    ))) %>% subset(name == "Not available" & !(code == ":"))

    if (nrow(na_locations) == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

  testable_levels_present <- data %>%
    filter(geographic_level %in% c(geography_matrix[c(4, 6:14), 1])) %>% # Removing country, region, la, rsc region and planning area/institution
    distinct(geographic_level) %>%
    pull(geographic_level)

  if (length(testable_levels_present) == 0) {
    output <- list(
      "message" = paste0("No applicable locations to test."),
      "result" = "IGNORE"
    )
    return(output)
  }

  singleLevelTidy <- function(value, level) {
    if (value == FALSE) {
      return()
    } else {
      return(level)
    }
  }

  if (length(testable_levels_present) == 1) {
    na_codes <- na_check(paste(testable_levels_present)) %>% singleLevelTidy(., testable_levels_present)
  } else {
    na_codes <- sapply(testable_levels_present, na_check) %>%
      stack() %>%
      filter(values == TRUE) %>%
      pull(ind)
  }

  if (length(na_codes) == 0) {
    output <- list(
      "message" = paste0("No tested locations have a name of 'Not available' without the corresponding code ':'."),
      "result" = "PASS"
    )
  } else {
    if (length(na_codes) == 1) {
      output <- list(
        "message" = paste0("The following geographic level has at least one location with a name of 'Not available', that does not have the corresponding code ':': '", paste0(na_codes), ". <br> - The code for 'Not available' should always be ':'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following geographic levels have at least one location with a a name of 'Not available', that does not have the corresponding code ':': '", paste0(na_codes, collapse = "', '"), "'. <br> - The code for 'Not available' should always be ':'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}


# col_name_duplicate -------------------------------------
# checking for duplicates in col_name

col_name_duplicate <- function(meta) {
  duplicated_col_names <- meta$col_name[duplicated(meta$col_name)]

  if (length(duplicated_col_names) == 0) {
    output <- list(
      "message" = "All col_name values are unique.",
      "result" = "PASS"
    )
  } else {
    if (length(duplicated_col_names) == 1) {
      output <- list(
        "message" = paste0("The following col_name value is duplicated in the metadata file: '", paste(duplicated_col_names), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following col_name values are duplicated in the metadata file: '", paste0(duplicated_col_names, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# col_name_spaces -------------------------------------
# check that no value in col_name has any spaces

col_name_spaces <- function(meta) {
  if (any(grepl("\\s", meta$col_name))) {
    output <- list(
      "message" = "There are spaces in the col_name values in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "There are no spaces in the col_name values.",
      "result" = "PASS"
    )
  }

  return(output)
}

# label -------------------------------------
# is label completed for every row

label <- function(meta) {
  completed_labels <- meta %>%
    filter(!is.na(label)) %>%
    nrow()

  blank_labels <- nrow(meta) - completed_labels

  if (blank_labels == 0) {
    output <- list(
      "message" = "The label column is completed for every row in the metadata.",
      "result" = "PASS"
    )
  } else {
    if (blank_labels == 1) {
      output <- list(
        "message" = paste0("There is a label missing in ", paste(blank_labels), " row of the metadata file."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("There are labels missing in ", paste(blank_labels), " rows of the metadata file."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# duplicate_label -------------------------------------
# checking for duplicate labels

duplicate_label <- function(meta) {
  duplicated_labels <- meta$label[duplicated(meta$label)]

  if (length(duplicated_labels) == 0) {
    output <- list(
      "message" = "All labels are unique.",
      "result" = "PASS"
    )
  } else {
    if (length(duplicated_labels) == 1) {
      output <- list(
        "message" = paste0("The following label is duplicated in the metadata file: '", paste(duplicated_labels), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following labels are duplicated in the metadata file: '", paste0(duplicated_labels, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# geographic_catch -------------------------------------
# catch if any geography columns are being put as filters (outside of ob units which are covered elsewhere)

geographic_catch <- function(meta) {
  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)

  filter_groups <- meta %>%
    filter(!is.na(filter_grouping_column)) %>%
    filter(filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  if (
    length(filters) == 1 &&
      any(filters[1] %in% geography_matrix[13:14, 3], filter_groups[1] %in% geography_matrix[13:14, 3])
  ) {
    filters_and_groups <- c(filters, filter_groups)[!c(filters, filter_groups) %in% c(geography_matrix[13, 3], geography_matrix[14, 3])]
  } else {
    filters_and_groups <- c(filters, filter_groups)
  }

  caught_filters <- filters_and_groups[grepl(potential_ob_units_regex, filters_and_groups, ignore.case = TRUE)]

  if (length(caught_filters) == 0) {
    output <- list(
      "message" = "No filters appear to be mislabelled geography columns.",
      "result" = "PASS"
    )
  } else {
    if (length(caught_filters) == 1) {
      output <- list(
        "message" = paste0("The following filter appears to be a geographic column and shouldn't be included in the metadata file: '", paste0(caught_filters, collapse = "', '"), "'. ", "<br> - <a href='mailto: explore.statistics@education.gov.uk'>Contact us</a>", " if you are unsure."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following filters appear to be geographic columns and shouldn't be included in the metadata file: '", paste0(caught_filters, collapse = "', '"), "'. ", "<br> - <a href='mailto: explore.statistics@education.gov.uk'>Contact us</a>", " if you are unsure."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# filter_hint -------------------------------------
# filter_hint should be blank for indicators

filter_hint <- function(meta) {
  filter_hints <- meta %>%
    filter(col_type == "Indicator", !is.na(filter_hint)) %>%
    pull(filter_hint)

  if (length(filter_hints) > 0) {
    output <- list(
      "message" = "Indicators should not have a filter_hint value in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "No indicators have an filter_hint value.",
      "result" = "PASS"
    )
  }

  return(output)
}

# filter_group -------------------------------------
# filter_grouping column is blank for all indicators

filter_group <- function(meta) {
  filter_groups <- meta %>%
    filter(col_type == "Indicator", !is.na(filter_grouping_column) & filter_grouping_column != "") %>%
    pull(filter_grouping_column)

  if (length(filter_groups) > 0) {
    output <- list(
      "message" = "Indicators should not have a filter_grouping_column value in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "No indicators have an filter_grouping_column value.",
      "result" = "PASS"
    )
  }

  return(output)
}

# filter_group_match -------------------------------------
# filter groups should be in the vector for column names for the data file

filter_group_match <- function(data, meta) {
  meta_filter_groups <- meta %>% filter(!is.na(filter_grouping_column) & filter_grouping_column != "")

  if (nrow(meta_filter_groups) == 0) {
    output <- list(
      "message" = "There are no filter groups present.",
      "result" = "IGNORE"
    )
  } else {
    filter_groups_not_in_data <- setdiff(meta_filter_groups$filter_grouping_column, names(data))
    number_filter_groups_not_in_data <- length(filter_groups_not_in_data)

    if (number_filter_groups_not_in_data == 0) {
      output <- list(
        "message" = "All filter groups from the metadata were found in the data file.",
        "result" = "PASS"
      )
    } else {
      if (number_filter_groups_not_in_data == 1) {
        output <- list(
          "message" = paste0("The following filter group from the metadata was not found as a variable in the data file: '", paste0(filter_groups_not_in_data, collapse = "', '"), "'."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The following filter groups from the metadata were not found as variables in the data file: '", paste0(filter_groups_not_in_data, collapse = "', '"), "'."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# filter_group_level -------------------------------------
# Checking that filter groups have fewer levels than their filters

filter_group_level <- function(data, meta) {
  meta_filters_and_groups <- meta %>%
    filter(col_type == "Filter", !is.na(filter_grouping_column) & filter_grouping_column != "") %>%
    select(col_name, filter_grouping_column)

  if (nrow(meta_filters_and_groups) == 0) {
    output <- list(
      "message" = "There are no filter groups present.",
      "result" = "IGNORE"
    )
  } else {
    get_levels <- function(i) {
      as_tibble(data) %>%
        pull(i) %>%
        unique() %>%
        length()
    }

    filter_levels <- stack(sapply(meta_filters_and_groups %>% pull(col_name), get_levels)) %>% rename("col_name" = "ind", "filter_levels" = "values")

    filter_group_levels <- stack(sapply(meta_filters_and_groups %>% pull(filter_grouping_column) %>% unique(), get_levels)) %>% rename("filter_grouping_column" = "ind", "group_levels" = "values")

    extended_meta <- suppressWarnings(suppressMessages(meta_filters_and_groups %>% inner_join(filter_levels) %>% inner_join(filter_group_levels) %>% mutate("pre_result" = case_when(filter_levels >= group_levels ~ "PASS", TRUE ~ "FAIL"))))

    failed_pairs <- extended_meta %>%
      filter(pre_result == "FAIL")

    number_of_failed_pairs <- failed_pairs %>%
      nrow()

    if (number_of_failed_pairs == 0) {
      output <- list(
        "message" = "All filter groups have an equal or lower number of levels than their corresponding filter.",
        "result" = "PASS"
      )
    } else {
      if (number_of_failed_pairs == 1) {
        output <- list(
          "message" = paste0("The filter group '", paste(failed_pairs$filter_grouping_column), "' has more levels (", paste(failed_pairs$group_levels), ") than its corresponding filter '", paste(failed_pairs$col_name), "' (", paste(failed_pairs$filter_levels), "). <br> - This suggests that the hierarchy is the wrong way around in the metadata."),
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = paste0("The following filter groups each have more levels than their corresponding filters, check that they are entered the correct way around in the metadata: <br> - '", paste0(failed_pairs$filter_grouping_column, collapse = "', '"), "'."),
          "result" = "FAIL"
        )
      }
    }
  }

  return(output)
}

# filter_group_not_filter -------------------------------------
# Checking that filter groups are not filters

filter_group_not_filter <- function(meta) {
  if (meta %>% filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>% nrow() == 0) {
    output <- list(
      "message" = "There are no filter groups present.",
      "result" = "IGNORE"
    )
  } else {
    filter_group_not_filter_check <- function(i) {
      if (i %in% meta$col_name) {
        return("FAIL")
      } else {
        return("PASS")
      }
    }

    pre_result <- stack(sapply(meta %>%
      filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>%
      pull(filter_grouping_column), filter_group_not_filter_check))

    filter_groups_in_col_names <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if ("FAIL" %in% pre_result$values) {
      output <- list(
        "message" = paste0("Filter groups should not appear in the col_name column in the metadata file. <br> - Please remove the following from col_name: '", paste(filter_groups_in_col_names, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "No filter groups are included in the col_name column.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# filter_group_duplicate -------------------------------------
# Checking that filter groups are not duplicated

filter_group_duplicate <- function(meta) {
  if (meta %>% filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>% nrow() == 0) {
    output <- list(
      "message" = "There are no filter groups present.",
      "result" = "IGNORE"
    )
  } else {
    if (suppressMessages(meta %>% filter(!is.na(filter_grouping_column) & filter_grouping_column != "") %>% get_dupes(filter_grouping_column) %>% nrow()) != 0) {
      output <- list(
        "message" = "There are duplicated filter_group values.",
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "All of the filter_group values are unique.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# whitespace_filters -------------------------------------
# check no filter labels have leading or trailing whitespace.

whitespace_filters <- function(data, meta) {
  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)

  test <- data %>%
    select(all_of(filters), any_of(as.character(geography_matrix[, 2:4]) %>% .[!is.na(.)])) %>%
    mutate_if(is.numeric, as.character) %>%
    pivot_longer(everything(), values_drop_na = TRUE, names_to = "filter", values_to = "filter_label") %>%
    # gather(., "filter", "filter_label") %>%
    distinct()

  test_no_whitespace <- test %>% mutate(filter_label = str_trim(filter_label))

  white_spaces <- setdiff(test, test_no_whitespace) %>% pull(filter_label)

  if (length(white_spaces) == 0) {
    output <- list(
      "message" = "No filter labels contain leading or trailing whitespace.",
      "result" = "PASS"
    )
  } else {
    if (length(white_spaces) == 1) {
      output <- list(
        "message" = paste0("The following filter label contains leading or trailing whitespace: '", paste0(white_spaces, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following filter labels contain leading or trailing whitespace: '", paste0(white_spaces, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}


# indicator_grouping -------------------------------------
# indicator grouping - should be blank for all filters

indicator_grouping <- function(meta) {
  indicator_groups <- meta %>%
    filter(col_type == "Filter", !is.na(indicator_grouping), indicator_grouping != "") %>%
    pull(indicator_grouping)

  if (length(indicator_groups) > 0) {
    output <- list(
      "message" = "Filters should not have an indicator_grouping value in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "No filters have an indicator_grouping value.",
      "result" = "PASS"
    )
  }

  return(output)
}
# filter_group_stripped -------------------------------------
# filter groups - should have the same number of unique filter groups when stripped of non-alphanumeric characters

filter_group_stripped <- function(data, meta) {
  if (meta %>% filter(filter_grouping_column != "") %>% nrow() == 0) {
    output <- list(
      "message" = "There are no filter groups present.",
      "result" = "IGNORE"
    )
  } else {
    filter_group_columns <- meta %>%
      filter(col_type == "Filter", filter_grouping_column != "") %>%
      pull(filter_grouping_column)

    get_values <- function(column) {
      return(unique(data[[column]]))
    }

    raw_filter_groups <- lapply(filter_group_columns, get_values)

    stripped_filter_groups <- lapply(raw_filter_groups, gsub, pattern = "[^[:alnum:]]", replacement = "") %>% lapply(unique)

    comparison <- unlist(lapply(raw_filter_groups, length)) == unlist(lapply(stripped_filter_groups, length))

    failed_cols <- which(comparison %in% FALSE)

    if (length(failed_cols) > 0) {
      output <- list(
        "message" = paste0("The number of unique filter groups should not change when non-alphanumeric characters are stripped. <br> - please check this list for erroneous filter group values: '", paste0(unlist(raw_filter_groups[failed_cols]), collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "There are no issues when stripping alpha-numeric characters from filter groups.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}

# indicator_group_stripped -------------------------------------
# indicator grouping - should have the same number of unique indicator groups when stripped of non-alphanumeric characters

indicator_group_stripped <- function(meta) {
  if (meta %>% filter(indicator_grouping != "") %>% nrow() == 0) {
    output <- list(
      "message" = "There are no indicator groups present.",
      "result" = "IGNORE"
    )
  } else {
    raw_indicator_groups <- meta %>%
      filter(col_type == "Indicator", indicator_grouping != "") %>%
      pull(indicator_grouping) %>%
      unique()

    stripped_indicator_groups <- lapply(raw_indicator_groups, gsub, pattern = "[^[:alnum:]]", replacement = "") %>%
      unique()

    if (length(raw_indicator_groups) != length(stripped_indicator_groups)) {
      output <- list(
        "message" = paste0("The number of unique indicator groups should not change when non-alphanumeric characters and spaces are stripped. <br> - please check this list for erroneous groups: '", paste0(raw_indicator_groups, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = "There are no issues when stripping alpha-numeric characters and spaces from indicator groups.",
        "result" = "PASS"
      )
    }
  }

  return(output)
}


# indicator_unit -------------------------------------
# indicator unit should be blank for all filters

indicator_unit <- function(meta) {
  indicator_units <- meta %>%
    filter(col_type == "Filter", !is.na(indicator_unit), indicator_unit != "") %>%
    pull(indicator_unit)

  if (length(indicator_units) > 0) {
    output <- list(
      "message" = "Filters should not have an indicator_unit value in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "No filters have an indicator_unit value.",
      "result" = "PASS"
    )
  }

  return(output)
}

# indicator_unit_validation -------------------------------------
# Validation for the indicator units

indicator_unit_validation <- function(meta) {
  present_indicatorunits <- filter(meta, col_type == "Indicator", !is.na(indicator_unit), indicator_unit != "") %>%
    pull(indicator_unit)

  invalid_indicatorunits <- setdiff(unique(present_indicatorunits), acceptable_indicatorunits)

  if (length(invalid_indicatorunits) == 0) {
    output <- list(
      "message" = "The indicator_unit values are valid.",
      "result" = "PASS"
    )
  } else {
    if (length(invalid_indicatorunits) == 1) {
      output <- list(
        "message" = paste0("The following invalid indicator unit is present in the metadata file: '", paste0(invalid_indicatorunits, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    } else {
      output <- list(
        "message" = paste0("The following invalid indicator units are present in the metadata file: '", paste0(invalid_indicatorunits, collapse = "', '"), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# indicator_dp -------------------------------------
# indicator_dp should be blank for all filters

indicator_dp <- function(meta) {
  indicator_dps <- meta %>%
    filter(col_type == "Filter", !is.na(indicator_dp), indicator_dp != "") %>%
    pull(indicator_dp)

  if (length(indicator_dps) > 0) {
    output <- list(
      "message" = "Filters should not have an indicator_dp value in the metadata file.",
      "result" = "FAIL"
    )
  } else {
    output <- list(
      "message" = "No filters have an indicator_dp value.",
      "result" = "PASS"
    )
  }

  return(output)
}

# indicator_dp_validation -------------------------------------
# indicator_dp should be numeric or blank

indicator_dp_validation <- function(meta) {
  if (all(is.na(meta$indicator_dp))) {
    output <- list(
      "message" = "The indicator_dp column only contains blanks.",
      "result" = "PASS"
    )
  } else {
    if (is.numeric(meta$indicator_dp)) {
      isInteger <- function(x) {
        test <- all.equal(x, as.integer(x), check.attributes = FALSE)
        if (test == TRUE) {
          return(TRUE)
        }
        else {
          return(FALSE)
        }
      }

      meta$integer <- lapply(meta$indicator_dp, isInteger)
      meta$notNegative <- lapply(meta$indicator_dp, function(x) x >= 0)
      failed_rows <- rbind(
        meta %>% filter(integer == FALSE),
        meta %>% filter(notNegative == FALSE)
      )

      if (nrow(failed_rows) != 0) {
        output <- list(
          "message" = "The indicator_dp column must only contain blanks or positive integer values.",
          "result" = "FAIL"
        )
      } else {
        output <- list(
          "message" = "The indicator_dp column only contains blanks or positive integer values.",
          "result" = "PASS"
        )
      }
    } else {
      output <- list(
        "message" = "The indicator_dp column must only contain numeric values or blanks in the metadata file.", # The following value is invalid: '", paste(invalid_values), "'."),
        "result" = "FAIL"
      )
    }
  }

  return(output)
}

# indicator_dp_completed -------------------------------------
# indicator_dp should be completed for indicators

indicator_dp_completed <- function(meta) {
  blankIndicators <- meta %>%
    filter(col_type == "Indicator") %>%
    filter(is.na(indicator_dp)) %>%
    pull(col_name)

  if (length(blankIndicators) == 0) {
    output <- list(
      "message" = "The indicator_dp column is completed for all indicators.",
      "result" = "PASS"
    )
  } else {
    if (length(blankIndicators) == 1) {
      output <- list(
        "message" = paste0(paste(blankIndicators, collapse = "', '"), " does not have a specified number of dp in the metadata file, this should be explicity stated where possible."),
        "result" = "ADVISORY"
      )
    } else {
      output <- list(
        "message" = paste0("The following indicators do not have a specified number of decimal places in the indicator_dp column of metadata file: <br> - '", paste(blankIndicators, collapse = "', '"), "'. <br> - These should be explicity stated where possible."),
        "result" = "ADVISORY"
      )
    }
  }

  return(output)
}
