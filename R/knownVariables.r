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

geography_matrix <- matrix(c(
  "National", "country_code", "country_name", NA, NA, NA,
  "Regional", "region_code", "region_name", NA, NA, NA,
  "Local authority", "old_la_code", "la_name", "new_la_code", NA, NA,
  "Local authority district", "lad_code", "lad_name", NA, NA, NA,
  "RSC region", "rsc_region_lead_name", NA, NA, NA, NA,
  "Parliamentary constituency", "pcon_code", "pcon_name", NA, NA, NA,
  "Local enterprise partnership", "local_enterprise_partnership_code", "local_enterprise_partnership_name", NA, NA, NA,
  "English devolved area", "english_devolved_area_code", "english_devolved_area_name", NA, NA, NA,
  "Opportunity area", "opportunity_area_code", "opportunity_area_name", NA, NA, NA,
  "Ward", "ward_code", "ward_name", NA, NA, NA,
  "MAT", "trust_id", "trust_name", NA, NA, NA,
  "Sponsor", "sponsor_id", "sponsor_name", NA, NA, NA,
  "School", "school_laestab", "school_name", "school_urn", "school_estab", "school_postcode",
  "Provider", "provider_urn", "provider_name", "provider_ukprn", "provider_upin", NA,
  "Institution", "institution_id", "institution_name", NA, NA, NA,
  "Planning area", "planning_area_name", "planning_area_code", NA, NA, NA
),
ncol = 6,
byrow = TRUE
)

countries <- read_csv("data/country.csv") # change this to database eventually
regions <- read_csv("data/regions.csv") # change this to database eventually
las <- read_csv("data/las.csv") # change this to database eventually

expected_country_combinations <- unique(paste(countries$country_code, countries$country_name))

expected_region_combinations <- unique(paste(regions$region_code, regions$region_name))

expected_la_combinations <- unique(paste(las$old_la_code, las$new_la_code, las$la_name))

acceptable_levels <- c(
  geography_matrix[, 1] %>% .[!is.na(.)]
)

acceptable_observational_units <- c(
  "time_period", "time_identifier", "geographic_level",
  unlist(geography_matrix[, 2:6]) %>% .[!is.na(.)]
)


potential_ob_units_regex <- "(^(sch|prov|inst|estab|reg|la|local|rsc|pcon|lep|mca|oa|ward|mat).*(name|code|urn|ukprn|number|upin|id)$)|(^(laestab|estab|sch|school|schools|prov|provider|providers|inst|institution|institutions|name|code|urn|ukprn|number|upin|id|region|la|lad|rsc|pcon|lep|mca|oa|ward|mat)$)"

col_to_level_lookup <- rbind(
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 2]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 3]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 4]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 5]),
  data.frame(levels = geography_matrix[, 1], cols = geography_matrix[, 6])
) %>%
  filter(!is.na(cols))

acceptable_indicatorunits <- c("£", "£m", "%", "pp")
