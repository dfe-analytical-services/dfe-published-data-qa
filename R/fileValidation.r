# fileValidation -------------------------------------
# File validation functions

fileValidation <- function(datafilename, metafilename, dataseparator, metaseparator, datafile, metafile) {
  as_tibble(t(rbind(
    cbind(
      data_filename_spaces(datafilename), # active test
      meta_filename_spaces(metafilename), # active test
      data_filename_special_characters(datafilename), # active test
      meta_filename_special_characters(metafilename), # active test
      naming_convention(datafilename, metafilename), # active test
      rows_to_cols(datafile, metafile), # active test
      file_separator(dataseparator, metaseparator), # active test
      data_empty_rows(datafile), # active test
      meta_empty_rows(metafile), # active test
      data_empty_cols(datafile), # active test
      data_mandatory_cols(datafile), # active test
      meta_mandatory_cols(metafile) # active test
    ),
    "stage" = "fileValidation",
    "test" = c(activeTests$`R/fileValidation.r`)
  )))
}

# data_filename_spaces -------------------------------------
# check that there are no spaces in the data file

data_filename_spaces <- function(data) {
  if (str_detect(data, " ")) {
    output <- list(
      "result" = "FAIL",
      "message" = "There are spaces that need removing in filename of the data file."
    )
  } else {
    output <- list(
      "result" = "PASS",
      "message" = "The data filename does not have spaces."
    )
  }

  return(output)
}

# meta_filename_spaces -------------------------------------
# check that there are no spaces in the metadata file

meta_filename_spaces <- function(meta) {
  if (str_detect(meta, " ")) {
    output <- list(
      "result" = "FAIL",
      "message" = "There are spaces that need removing in filename of the metadata file."
    )
  } else {
    output <- list(
      "result" = "PASS",
      "message" = "The metadata filename does not have spaces."
    )
  }

  return(output)
}

# data_filename_special_characters -------------------------------------
# check that there are no special characters in the data filename

data_filename_special_characters <- function(data) {
  if (grepl("[^[:alnum:]]", gsub("-|_|\\s", "", file_path_sans_ext(data)))) {
    present_special_characters <- unique(unlist(str_split(gsub("[A-Za-z0-9]|-|_|\\s", "", file_path_sans_ext(data)), ""), use.names = FALSE))

    output <- list(
      "result" = "FAIL",
      "message" = paste0("The following special characters need removing from the filename of the data file: ", paste0(present_special_characters, collapse = " "), ". <br> - Filenames must only contain numbers, letters, hyphens or underscores.")
    )
  } else {
    output <- list(
      "result" = "PASS",
      "message" = "The data filename does not have any special characters."
    )
  }

  return(output)
}

# meta_filename_special_characters -------------------------------------
# check that there are no special characters in the metadata filename

meta_filename_special_characters <- function(meta) {
  if (grepl("[^[:alnum:]]", gsub("-|_|\\s", "", gsub("\\.meta\\.csv$", "", meta)))) {
    present_special_characters <- unique(unlist(str_split(gsub("[A-Za-z0-9]|-|_|\\s", "", gsub("\\.meta\\.csv$", "", meta)), ""), use.names = FALSE))

    output <- list(
      "result" = "FAIL",
      "message" = paste0("The following special characters need removing from the filename of the metadata file: ", paste0(present_special_characters, collapse = " "), ". <br> - Filenames must only contain numbers, letters, hyphens or underscores.")
    )
  } else {
    output <- list(
      "result" = "PASS",
      "message" = "The metadata filename does not have any special characters."
    )
  }

  return(output)
}

# naming_convention -------------------------------------
# check that the files match the file naming conventions and can be recognised

naming_convention <- function(data, meta) {
  if (file_ext(data) != "csv" && file_ext(meta) != "csv") {
    output <- list(
      "result" = "FAIL",
      "message" = "Neither file is saved as a CSV. Please save your files as CSV's and re-upload."
    )
  } else {
    if (file_ext(data) != "csv") {
      output <- list(
        "result" = "FAIL",
        "message" = "The data file is not a CSV. Please save the file as a CSV and re-upload."
      )
    } else {
      if (file_ext(meta) != "csv") {
        output <- list(
          "result" = "FAIL",
          "message" = "The metadata file is not a CSV. Please save the file as a CSV and re-upload."
        )
      } else {
        if (meta == str_replace(data, "\\.csv", "\\.meta.csv")) {
          output <- list(
            "result" = "PASS",
            "message" = "The names of the files follow the recommended naming convention."
          )
        } else {
          output <- list(
            "result" = "FAIL",
            "message" = paste0("The filenames do not follow the recommended naming convention. <br> - Based on the given data filename, the metadata filename is expected to be '", paste(str_replace(data, ".csv", ".meta.csv'")))
          )
        }
      }
    }
  }

  return(output)
}

# rows_to_cols -------------------------------------
# rows in meta < cols in data file

rows_to_cols <- function(data, meta) {
  # 5 is the number of mandatory observational unit columns, so every data file should have at least 5 columns not included in the metadata

  data_cols <- ncol(data) - 5
  meta_rows <- nrow(meta)

  # give an extra explanation on the non-mandatory (tool tip?)
  # add extra detail as to where to look (both in col_name and filter_grouping_column)

  if (data_cols < meta_rows) {
    output <- list(
      "result" = "FAIL",
      "message" = paste0("There are more rows in the metadata file (", paste(comma_sep(meta_rows)), ") than the number of non-mandatory columns in the data file (", paste(comma_sep(data_cols)), "). <br> - This is not expected, please check the csv files. It can often be helpful open them in a text editor such as wordpad to investigate.")
    )
  } else {
    if (data_cols == meta_rows) {
      output <- list(
        "result" = "PASS",
        "message" = paste0("There are an equal number of rows in the metadata file (", paste(comma_sep(meta_rows)), ") and non-mandatory columns in the data file (", paste(comma_sep(data_cols)), ").")
      )
    } else {
      output <- list(
        "result" = "PASS",
        "message" = paste0("There are fewer rows in the metadata file (", paste(comma_sep(meta_rows)), ") than non-mandatory columns in the data file (", paste(comma_sep(data_cols)), ").")
      )
    }
  }

  return(output)
}

# file_separator -------------------------------------
# check for use of a comma as the main separator/delimiter

file_separator <- function(data, meta) {
  if (grepl(",", data) && grepl(",", meta)) {
    output <- list(
      "result" = "PASS",
      "message" = "The data and metadata files both appear to use a comma delimited structure."
    )
  } else {
    if ((grepl(",", data) == FALSE) && (grepl(",", meta) == FALSE)) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("Neither of the data and metadata files appear to be using a comma delimited structure. <br> - See the ", "<a href='https://dfe-analytical-services.github.io/analysts-guide/statistics-production/ud.html#data_format' target='_blank'>guidance on how to save the files</a>", " if you are unsure.")
      )
    } else {
      if (grepl(",", meta)) {
        output <- list(
          "result" = "FAIL",
          "message" = paste0("The data file does not appear to be using a comma delimited structure. <br> - See the ", "<a href='https://dfe-analytical-services.github.io/analysts-guide/statistics-production/ud.html#data_format' target='_blank'>guidance on how to save the files</a>", " if you are unsure.")
        )
      } else {
        output <- list(
          "result" = "FAIL",
          "message" = paste0("The metadata file does not appear to be using a comma delimited structure. <br> - See the ", "<a href='https://dfe-analytical-services.github.io/analysts-guide/statistics-production/ud.html#data_format' target='_blank'>guidance on how to save the files</a>", " if you are unsure.")
        )
      }
    }
  }

  return(output)
}

# data_empty_rows -------------------------------------
# check for blank rows in the data file

data_empty_rows <- function(data) {
  blank_rows <- nrow(data) - nrow(remove_empty(data, which = "rows", quiet = TRUE))

  if (blank_rows == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "The data file does not have any blank rows."
    )
  } else {
    if (blank_rows == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste("There is", comma_sep(blank_rows), "blank row in the data file. Try opening the csv in notepad if you're not sure where the blank rows are.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste("There are", comma_sep(blank_rows), "blank rows in the data file. Try opening the csv in notepad if you're not sure where the blank rows are.")
      )
    }
  }

  return(output)
}

# meta_empty_rows -------------------------------------
# check for blank rows in the metadata file

meta_empty_rows <- function(meta) {
  blank_rows <- nrow(meta) - nrow(remove_empty(meta, which = "rows", quiet = TRUE))

  if (blank_rows == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "The metadata file does not have any blank rows."
    )
  } else {
    if (blank_rows == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste("There is", comma_sep(blank_rows), "blank row in the metadata file. Try opening the csv in notepad if you're not sure where the blank rows are.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste("There are", comma_sep(blank_rows), "blank rows in the metadata file. Try opening the csv in notepad if you're not sure where the blank rows are.")
      )
    }
  }

  return(output)
}

# data_empty_cols -------------------------------------
# check for blank cols in the data file

data_empty_cols <- function(data) {
  blank_cols <- setdiff(names(data), names(remove_empty(data, which = "cols", quiet = TRUE)))

  if (length(blank_cols) == 0) {
    output <- list(
      "result" = "PASS",
      "message" = "The data file does not have any blank columns"
    )
  } else {
    if (length(blank_cols) == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following column in the data file is empty: '", paste0(blank_cols, collapse = "', '"), "'.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following columns in the data file are empty: '", paste0(blank_cols, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}

# data_mandatory_cols -------------------------------------
# check that the mandatory columns are present in the data file

data_mandatory_cols <- function(data) {
  mandatory_data_col_check <- function(i) {
    if (i %in% names(data)) {
      return("PASS")
    } else {
      return("FAIL")
    }
  }

  pre_result <- stack(sapply(mandatory_data_cols, mandatory_data_col_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "result" = "PASS",
      "message" = "All of the mandatory columns are present in the data file."
    )
  } else {
    missing_cols <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(missing_cols) == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following mandatory column is missing from the data file: '", paste(missing_cols, collapse = "', '"), "'.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following mandatory columns are missing from the data file: '", paste(missing_cols, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}

# meta_mandatory_cols -------------------------------------
# check that the mandatory columns are present in the metadata file

meta_mandatory_cols <- function(meta) {
  output <- list("test" = "meta_mandatory_cols")

  meta_col_check <- function(i) {
    if (i %in% names(meta)) {
      return("PASS")
    } else {
      return("FAIL")
    }
  }

  pre_result <- stack(sapply(mandatory_meta_cols, meta_col_check))

  if (all(pre_result$values == "PASS")) {
    output <- list(
      "result" = "PASS",
      "message" = "All of the mandatory columns are present in the metadata file."
    )
  } else {
    missing_cols <- filter(pre_result, values == "FAIL") %>% pull(ind)

    if (length(missing_cols) == 1) {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following mandatory column is missing from the metadata file: '", paste(missing_cols, collapse = "', '"), "'.")
      )
    } else {
      output <- list(
        "result" = "FAIL",
        "message" = paste0("The following mandatory columns are missing from the metadata file: '", paste(missing_cols, collapse = "', '"), "'.")
      )
    }
  }

  return(output)
}
