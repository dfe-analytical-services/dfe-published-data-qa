# Code for updating geography lookups =================================
# source functions and dependencies

source("R/standard-data-prep/utils.R")

# PCON LA =====================================================================
# Download latest from: https://geoportal.statistics.gov.uk/search?q=LUP_WPC&sort=Date%20Created%7Ccreated%7Cdesc
# ...and save into /data/downloaded_source_data
# Then add the filepath into the function below to update the lookup to
# ...account for any recent additions

# Last title of file used on Open Geography Portal (incase it's helpful):
# Ward to Westminster Parliamentary Constituency to Local Authority District to UTLA

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "data/downloaded_source_data/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_to_UTLA.csv",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  lookup_filepath = "data/la_pcon_hierarchy.csv"
)

# LAD LSIP ====================================================================
# Download latest from: https://geoportal.statistics.gov.uk/datasets/effcab9660fd4375baaed44c2bd23719_0/explore?q=local%20skills%20lad
# ...and save into /data/downloaded_source_data
# Then add the filepath into the function below to update the lookup to
# ...account for any recent additions

# Last title of file used on Open Geography Portal (incase it's helpful):
# LAD to Local skills improvement plan areas (August 2023) Lookup in England

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "data/downloaded_source_data/LAD_to_Local_skills_improvement_plan_areas_(August_2023)_Lookup_in_England.csv",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  lookup_filepath = "data/lsip_lad_hierarchy.csv"
)

# Ward LAD ====================================================================
# Download latest from: https://geoportal.statistics.gov.uk/search?collection=dataset&q=ward%20lad
# ...and save into /data/downloaded_source_data
# Then add the filepath into the function below to update the lookup to
# ...account for any recent additions

# Last title of file used on Open Geography Portal (incase it's helpful):
# Ward to Westminster Parliamentary Constituency to Local Authority District to UTLA (same as for pcon_la)

write_updated_lookup(
  new_lookup = tidy_downloaded_lookup(
    open_geography_file = "data/downloaded_source_data/2016.csv",
    shorthand_lookup = open_geog_shorthand_lookup
  ),
  lookup_filepath = "data/ward_lad_hierarchy.csv"
)
