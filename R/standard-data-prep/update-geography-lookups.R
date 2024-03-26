# Code for updating geography lookups =================================
# source functions and dependencies

source("R/standard-data-prep/utils.R")

# Create a lookup table for shorthand to levels we care about

open_geog_shorthands <- c("WD", "PCON", "LAD", "UTLA", "LSIP")
name_column <- paste0(c("ward", "pcon", "lad", "la", "lsip"), "_name")
code_column <- paste0(c("ward", "pcon", "lad", "new_la", "lsip"), "_code")

open_geog_shorthand_lookup <- data.frame(
  open_geog_shorthands,
  name_column,
  code_column
)

# PCON LA =====================================================================
# Download latest from:
# Last title of file on Open Geography Portal: 

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "data/downloaded_source_data/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_to_Upper_Tier_L.csv",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  lookup_filepath = "data/la_pcon_hierarchy.csv"
)

# LAD LSIP ====================================================================

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  
  lookup_filepath = "data/la_pcon_hierarchy.csv"
)

# Ward LAD ====================================================================

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  
  lookup_filepath = "data/la_pcon_hierarchy.csv"
)
