library(readr)
library(dplyr)
# @param open_geography_file Lookup file sourced from
#       https://geoportal.statistics.gov.uk/search?q=LUP_WPC&sort=Date%20Created%7Ccreated%7Cdesc
#       e.g. data/downloaded_source_data/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_to_Upper_Tier_L.csv
update_pcon_la_lookup <- function(
    open_geography_file,
    output_lookup = "data/la_pcon_hierarchy.csv",
    existing_data_file = "data/la_pcon_hierarchy.csv") {
  message("Reading in new data from: ", open_geography_file)
  new_data <- read_csv(open_geography_file, show_col_types = FALSE)
  new_year <- names(new_data) %>%
    as.data.frame() %>%
    filter(grepl("PCON", .), grepl("CD", .)) %>%
    pull(.) %>%
    gsub("PCON", "", .) %>%
    gsub("CD", "", .)
  new_lookup <- new_data %>%
    rename(
      pcon_code = paste0("PCON", new_year, "CD"),
      pcon_name = paste0("PCON", new_year, "NM"),
      lad_code = paste0("LAD", new_year, "CD"),
      lad_name = paste0("LAD", new_year, "NM"),
      new_la_code = paste0("UTLA", new_year, "CD"),
      la_name = paste0("UTLA", new_year, "NM")
    ) %>%
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
    ) %>%
    select(
      pcon_code,
      pcon_name,
      new_la_code,
      la_name
    ) %>%
    distinct() %>%
    mutate(
      first_available_year_included = paste0("20", new_year),
      most_recent_year_included = paste0("20", new_year)
    )
  if (!is.null(existing_data_file)) {
    message("Reading data from existing file: ", existing_data_file)
    existing_lookup <- read_csv(existing_data_file, show_col_types = FALSE)
    new_lookup <- existing_lookup %>%
      rbind(new_lookup) %>%
      summarise(
        first_available_year_included = min(first_available_year_included),
        most_recent_year_included = max(most_recent_year_included),
        .by = c("pcon_code", "pcon_name", "new_la_code", "la_name")
      )
  }
  message("Writing updated lookup to: ", output_lookup)
  new_lookup %>%
    arrange(pcon_code, new_la_code) %>%
    filter(grepl("E", pcon_code)) %>% # Filter to only include England
    write.csv(output_lookup, row.names = FALSE)
}

# @param open_geography_file Lookup file sourced from
#       https://geoportal.statistics.gov.uk/search?q=lsip&sort=Title%7Ctitle%7Casc
update_lad_lsip_lookup <- function(
    open_geography_file,
    output_lookup = "data/lsip_lad_hierarchy.csv",
    existing_data_file = "data/lsip_lad_hierarchy.csv") {
  message("Reading in new data from: ", open_geography_file)
  new_data <- read_csv(open_geography_file, show_col_types = FALSE)
  new_year <- names(new_data) %>%
    as.data.frame() %>%
    filter(grepl("LSIP", .), grepl("CD", .)) %>%
    pull(.) %>%
    gsub("LSIP", "", .) %>%
    gsub("CD", "", .)
  new_lookup <- new_data %>%
    select(
      lad_code = paste0("LAD", new_year, "CD"),
      lad_name = paste0("LAD", new_year, "NM"),
      lsip_code = paste0("LSIP", new_year, "CD"),
      lsip_name = paste0("LSIP", new_year, "NM"),
    ) %>%
    distinct() %>%
    mutate(
      first_available_year_included = paste0("20", new_year),
      most_recent_year_included = paste0("20", new_year)
    )
  if (!is.null(existing_data_file)) {
    message("Reading data from existing file: ", existing_data_file)
    existing_lookup <- read_csv(existing_data_file, show_col_types = FALSE)
    new_lookup <- existing_lookup %>%
      rbind(new_lookup) %>%
      summarise(
        first_available_year_included = min(first_available_year_included),
        most_recent_year_included = max(most_recent_year_included),
        .by = c("lad_code", "lad_name", "lsip_code", "lsip_name")
      )
  }
  message("Writing updated lookup to: ", output_lookup)
  new_lookup %>%
    arrange(lsip_code, lad_code) %>%
    write.csv(output_lookup, row.names = FALSE)
}
