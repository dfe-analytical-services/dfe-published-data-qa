# Defining the hard-coded variables -------------------------------------

mandatory_data_cols <- c("geographic_level", "time_period", "time_identifier", "country_code", "country_name")

mandatory_meta_cols <- c("col_name", "col_type", "label", "indicator_grouping", "indicator_unit", "indicator_dp", "filter_hint", "filter_grouping_column")

terms <- c("Spring term", "Autumn term", "Summer term")

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

weeks <- c(
  "Week 1", "Week 2", "Week 3", "Week 4", "Week 5", "Week 6", "Week 7", "Week 8", "Week 9", "Week 10",
  "Week 11", "Week 12", "Week 13", "Week 14", "Week 15", "Week 16", "Week 17", "Week 18", "Week 19", "Week 20",
  "Week 21", "Week 22", "Week 23", "Week 24", "Week 25", "Week 26", "Week 27", "Week 28", "Week 29", "Week 30",
  "Week 31", "Week 32", "Week 33", "Week 34", "Week 35", "Week 36", "Week 37", "Week 38", "Week 39", "Week 40",
  "Week 41", "Week 42", "Week 43", "Week 44", "Week 45", "Week 46", "Week 47", "Week 48", "Week 49", "Week 50",
  "Week 51", "Week 52"
)

financial_quarters <- c("Financial year Q1", "Financial year Q2", "Financial year Q3", "Financial year Q4")

financial_halves <- c("Part 1 (April to September)", "Part 2 (October to March)")

acceptable_time_identifiers <- c(
  "Autumn and spring term", "Calendar year", "Financial year", "Academic year", "Tax year", "Reporting year",
  terms, weeks, months, financial_quarters, financial_halves
)

four_digit_identifiers <- c("Calendar year", "Reporting year", weeks, months)

six_digit_identifiers <- c("Autumn and spring term", terms, "Financial year", "Academic year", "Tax year", financial_quarters, financial_halves)

# Latest GSS/ONS data symbols
gssNAvcode <- "x" # Note that this one is also hardwired into the las.csv file for some terminated LAs that don't have a code listed.
gssNApcode <- "z"
gssSupcode <- "c"
gssRndcode <- "low"
gss_symbols <- c(gssNApcode, gssNAvcode, gssSupcode, gssRndcode)
legacy_gss_symbols <- c("~", ":")

geography_matrix <- matrix(
  c(
    "National", "country_code", "country_name", NA,
    "Regional", "region_code", "region_name", NA,
    "Local authority", "old_la_code", "la_name", "new_la_code",
    "Local authority district", "lad_code", "lad_name", NA,
    "RSC region", "rsc_region_lead_name", NA, NA,
    "Parliamentary constituency", "pcon_code", "pcon_name", NA,
    "Local skills improvement plan area", "lsip_code", "lsip_name", NA,
    "Local enterprise partnership", "local_enterprise_partnership_code", "local_enterprise_partnership_name", NA,
    "English devolved area", "english_devolved_area_code", "english_devolved_area_name", NA,
    "Opportunity area", "opportunity_area_code", "opportunity_area_name", NA,
    "Ward", "ward_code", "ward_name", NA,
    "MAT", "trust_id", "trust_name", NA,
    "Sponsor", "sponsor_id", "sponsor_name", NA,
    "School", "school_urn", "school_name", "school_laestab",
    "Provider", "provider_ukprn", "provider_name", NA,
    "Institution", "institution_id", "institution_name", NA,
    "Planning area", "planning_area_code", "planning_area_name", NA
  ),
  ncol = 4,
  byrow = TRUE
)

geography_dataframe <- geography_matrix %>%
  as.data.frame() %>%
  select(geographic_level = V1, code_field = V2, name_field = V3, code_field_secondary = V4)

# Pull out lower level geographies that we don't have standardised lists for (and excluding school-provider etc levels)
lower_level_geog_levels <- c("Opportunity area", "MAT", "Sponsor")
lower_level_geog_names <- geography_dataframe %>%
  filter(geographic_level %in% lower_level_geog_levels) %>%
  pivot_longer(c(code_field, name_field)) %>%
  pull(value)

# Change all of these to database eventually
countries <- suppressMessages(read_csv("data/country.csv"))
regions <- suppressMessages(read_csv("data/regions.csv"))
lsips <- suppressMessages(read_csv("data/lsips.csv"))
wards <- suppressMessages(read_csv("data/ward_lad_la_pcon_hierarchy.csv"))
las <- suppressMessages(read_csv("data/las.csv"))
lads <- suppressMessages(read_csv("data/lads.csv"))
leps <- suppressMessages(read_csv("data/leps.csv"))
edas <- suppressMessages(read_csv("data/english_devolved_areas.csv"))

# PCons, this is quick and dirty, should move to use the ward / PCon / LAD / LA file instead
pcons <- suppressMessages(read_csv("data/pcons.csv"))
pcons_2024 <- suppressMessages(read_csv("data/pcon_2024_v2.csv")) %>%
  select(pcon_code = PCON24CD, pcon_name = PCON24NM)

combined_pcons <- rbind(pcons %>% select(pcon_code, pcon_name), pcons_2024)

# Create combinations to use in tests
expected_country_combinations <- unique(paste(countries$country_code, countries$country_name))

expected_region_combinations <- unique(paste(regions$region_code, regions$region_name))

expected_lsip_combinations <- unique(paste(lsips$lsip_code, lsips$lsip_name))

expected_ward_combinations <- unique(paste(wards$ward_code, wards$ward_name))

expected_la_combinations <- unique(paste(las$old_la_code, las$new_la_code, las$la_name))

expected_lad_combinations <- unique(paste(lads$lad_code, lads$lad_name))

expected_pcon_combinations <- unique(paste(combined_pcons$pcon_code, combined_pcons$pcon_name))

expected_lep_combinations <- unique(paste(leps$local_enterprise_partnership_code, leps$local_enterprise_partnership_name))

expected_eda_combinations <- unique(paste(edas$english_devolved_area_code, edas$english_devolved_area_name))

acceptable_levels <- c(
  geography_matrix[, 1] %>% .[!is.na(.)]
)

acceptable_observational_units <- c(
  "time_period", "time_identifier", "geographic_level",
  unlist(geography_matrix[, 2:4]) %>% .[!is.na(.)]
)

potential_ob_units_regex <- "(^(sch|prov|inst|estab|reg|la|local|rsc|pcon|lep|mca|oa|ward|mat).*(name|code|urn|ukprn|number|upin|id)$)|(^(laestab|estab|sch|school|schools|prov|provider|providers|inst|institution|institutions|name|code|urn|ukprn|number|upin|id|region|la|lad|rsc|pcon|lep|mca|oa|ward|mat)$)"

col_to_level_lookup <- rbind(
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 2]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 3]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 4])
) %>%
  filter(!is.na(cols))

acceptable_indicatorunits <- c("%", "pp", "£", "£m")

ethnicity_standard_values <- suppressMessages(read_csv("data/ethnicity.csv")) # change this to database eventually
