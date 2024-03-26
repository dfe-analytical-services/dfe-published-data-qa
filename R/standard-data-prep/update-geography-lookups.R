# Example code for updating geography lookups =================================
# source and use the functions in the /R/standard-data-prep/utils.R file to

source("R/standard-data-prep/utils.R")

# PCON LA =====================================================================

new_pcon_la_file <- create_new_lookup(
  # some stuff
) %>%
  # Manual faffing specific to this file
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
  )

write_updated_lookup(
  # some stuff
)

# LAD LSIP ====================================================================

new_lad_lsip_file <- create_new_lookup(
  # some stuff
)

write_updated_lookup(
  # some stuff
)

# Ward LAD ====================================================================

new_ward_lad_file <- create_new_lookup(
  # some stuff
)

write_updated_lookup(
  # some stuff
)
