# Code that was in app, dumped here for now

# Checking if numbers add up
# This is just an example based on passes_everything and will need tons of work to make sure it's flexible enough for any data file
output$agg_check <- renderTable({
  data <- data$mainFile %>%
    group_by(geographic_level, time_period, school_phase, school_type) %>%
    summarise(aggregate_number = sum(enrolments)) %>% # sum(get(enrolments))) %>%
    filter(school_type == "State-funded primary" & school_phase == "Individual") %>%
    pivot_wider(names_from = geographic_level, values_from = aggregate_number) # %>%
  # spread(key = geographic_level, value = aggregate_number) %>%
  # select(-school_type)
  
  check <- function(dataset, id = "time_period") {
    years <- dataset[, id]
    dataset[, id] <- NULL
    dataset$match <- do.call(pmax, as.list(dataset)) == do.call(pmin, as.list(dataset))
    dataset[, id] <- years
    
    dataset <- dataset %>%
      select(time_period, everything()) %>%
      mutate(match = ifelse(match == TRUE, "MATCH", "NO MATCH"))
    
    return(dataset)
  }
  
  output_data <- check(data)
  
  return(output_data)
})




# filterArguments and otherIndicators are used in both indicatorSummaryStatistics and compareNationalAggregations - refactor?

# Need to refactor the stages, make it flow better and should make it more descriptive

#### Creating the indicator summary stats ----

indicatorSummaryStatistics <- function(data, meta, indicator) {
  filterArguments <- paste0(meta %>% filter(col_type == "Filter") %>% pull(col_name), collapse = ", ")
  otherIndicators <- meta %>%
    filter(col_type == "Indicator") %>%
    pull(col_name) %>%
    setdiff(x = ., y = indicator)

  stage1 <- data %>% select(-c(all_of(otherIndicators)))

  stage2 <- eval(parse(text = paste0("stage1 %>% filter(!", indicator, " %in% c('c', 'z', '~'))")))

  # Stage 2 only helps if every year has a symbol, if some years have data and others don't, an NA will be forced in - should fix

  stage3 <- eval(parse(text = paste0("stage2 %>% mutate(", indicator, "= as.numeric(", indicator, "))")))

  stage4 <- eval(parse(text = paste0("group_by(.data = stage3, time_period, geographic_level, ", filterArguments, ")")))

  stage5 <- eval(parse(text = paste0(
    "stage4 %>% summarise(min = min(", indicator, "), max = max(", indicator, "),mean = round(mean(", indicator,
    ")), q1 = quantile(", indicator, ", 0.25), median = median(", indicator, "), q3 = quantile(", indicator, ", 0.75))"
  )))

  stage6 <- eval(parse(text = paste0("stage5 %>% arrange(match(geographic_level, c('National', 'Regional', 'Local authority')), desc(time_period), ", filterArguments, ")")))

  # Would need to apply something like this over the summary stats cols, can worry about at a later date
  # stage7 <- eval(parse(text= paste0("data[[",indicator,"]] <- paste0(data[[",indicator,"]], meta %>% filter(col_name == ",indicator,") %>% pull(indicator_unit))")))

  return(stage6)
}

##### Compare geographical aggregations (maybe useful in the future?) -----

compareNationalAggregations <- function(data, qa, meta, indicator) {
  filterArguments <- paste0(meta %>% filter(col_type == "Filter") %>% pull(col_name), collapse = ", ")
  otherIndicators <- meta %>%
    filter(col_type == "Indicator") %>%
    pull(col_name) %>%
    setdiff(x = ., y = indicator)

  # Create national summary from main data ===================================================================================
  nationalStage1 <- data %>%
    select(-all_of(otherIndicators)) %>%
    filter(geographic_level == "National")

  nationalStage1.5 <- eval(parse(text = paste0("nationalStage1 %>% filter(", indicator, " != 'c')")))

  nationalStage2 <- eval(parse(text = paste0("nationalStage1.5 %>% group_by(time_period,", filterArguments, ")")))

  nationalStage3 <- eval(parse(text = paste0("nationalStage2 %>% mutate(", indicator, " = as.numeric(", indicator, "))")))

  nationalStage3 <- eval(parse(text = paste0("nationalStage3 %>% summarise(national = sum(", indicator, "))")))

  nationalSummarised <- nationalStage3 %>% arrange(desc(time_period)) # would rather descending by filter maybe to bring total to top? Worry about later

  # Create regional summary from raw qa data ==================================================================================

  regionalStage1 <- qa %>%
    filter(geographic_level == "Regional") %>%
    filter(region_name != "London") # dropping the full london aggregation so that it doesn't double the london values

  regionalStage2 <- eval(parse(text = paste0("regionalStage1 %>% select(time_period, region_name, ", filterArguments, paste0(", raw_", indicator), ")")))

  regionalStage3 <- eval(parse(text = paste0("regionalStage2 %>% rename(", indicator, " = ", paste0("raw_", indicator), ")")))

  regionalStage4 <- eval(parse(text = paste0("regionalStage3 %>% group_by(time_period,", filterArguments, ")")))

  regionalStage5 <- eval(parse(text = paste0("regionalStage4 %>% mutate(", indicator, " = as.numeric(", indicator, "))")))

  regionalStage6 <- eval(parse(text = paste0("regionalStage5 %>% summarise(regional = sum(", indicator, "))")))

  regionalSummarised <- regionalStage6 %>% arrange(desc(time_period)) # would rather descending by filter maybe to bring total to top? Worry about later

  # Create la summary from raw qa data ========================================================================================

  laStage1 <- qa %>%
    filter(geographic_level == "Local authority")

  laStage2 <- eval(parse(text = paste0("laStage1 %>% select(time_period, region_name, ", filterArguments, paste0(", raw_", indicator), ")")))

  laStage3 <- eval(parse(text = paste0("laStage2 %>% rename(", indicator, " = ", paste0("raw_", indicator), ")")))

  laStage4 <- eval(parse(text = paste0("laStage3 %>% group_by(time_period,", filterArguments, ")")))

  laStage5 <- eval(parse(text = paste0("laStage4 %>% mutate(", indicator, " = as.numeric(", indicator, "))")))

  laStage6 <- eval(parse(text = paste0("laStage5 %>% summarise(localAuthority = sum(", indicator, "))")))

  laSummarised <- laStage6 %>% arrange(desc(time_period)) # would rather descending by filter maybe to bring total to top? Worry about later

  # Join and compare the summaries =============================================================================================

  joinedSummaries <- nationalSummarised %>%
    left_join(regionalSummarised) %>%
    mutate(regionalMatchNational = if_else(national == roundFiveUp(regional, -1), TRUE, FALSE)) %>%
    left_join(laSummarised) %>%
    mutate(
      laMatchNational = if_else(national == roundFiveUp(localAuthority, -1), TRUE, FALSE),
      laMatchRegional = if_else(localAuthority == regional, TRUE, FALSE)
    )

  # Output table ===============================================================================================================

  return(joinedSummaries)
}

#### Count of unique locations per year ----

geographySummary <- rbind(
  countriesPresent %>% rename(location_code = country_code) %>% select(-country_name) %>% mutate("geographic_level" = "National"),
  regionsPresent %>% rename(location_code = region_code) %>% select(-region_name) %>% mutate("geographic_level" = "Regional"),
  lasPresent %>% rename(location_code = new_la_code) %>% select(-c(old_la_code, la_name)) %>% mutate("geographic_level" = "Local authority")
) %>%
  group_by(time_period, geographic_level) %>%
  tally() %>%
  rename(unique_locations = n) %>%
  arrange(desc(time_period), match(geographic_level, c("National", "Regional", "Local authority"))) %>%
  pivot_wider(names_from = time_period, values_from = unique_locations)

knitr::kable(geographySummary)


#####################################################################################################################################################################################
#### THIS BLOCK IS BEYOND HORRIFIC - CAN DEFINITELY BE REFACTORED AND IMPROVED. A LOT OF DUPLICATION ################################################################################
#####################################################################################################################################################################################

## LA suppression statistics ========================================================================================================================================================

primaryLA <- qaFile %>%
  select(-c(
    raw_number_of_leavers, secondary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "Local authority")

totalLA <- qaFile %>%
  select(-c(
    raw_number_of_leavers, primary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "Local authority")

totalCellsLA <- primaryLA %>% nrow()

primarySuppressedLA <- primaryLA %>%
  filter(primary_number_of_leavers == "c") %>%
  nrow()

primarySuppressedLAPercentage <- paste0(roundFiveUp(100 * primarySuppressedLA / totalCellsLA, 2), "%")

totalSuppressedLA <- totalLA %>%
  filter(secondary_number_of_leavers == "c") %>%
  nrow()

totalSuppressedLAPercentage <- paste0(roundFiveUp(100 * totalSuppressedLA / totalCellsLA, 2), "%")

secondarySuppressedLA <- totalSuppressedLA - primarySuppressedLA

secondarySuppressedLAPercentage <- paste0(roundFiveUp(100 * secondarySuppressedLA / totalCellsLA, 2), "%")

## Regional suppression statistics ========================================================================================================================================================

primaryRegional <- qaFile %>%
  select(-c(
    raw_number_of_leavers, secondary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "Regional")

totalRegional <- qaFile %>%
  select(-c(
    raw_number_of_leavers, primary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "Regional")

totalCellsRegional <- primaryRegional %>% nrow()

primarySuppressedRegional <- primaryRegional %>%
  filter(primary_number_of_leavers == "c") %>%
  nrow()

primarySuppressedRegionalPercentage <- paste0(roundFiveUp(100 * primarySuppressedRegional / totalCellsRegional, 2), "%")

totalSuppressedRegional <- totalRegional %>%
  filter(secondary_number_of_leavers == "c") %>%
  nrow()

totalSuppressedRegionalPercentage <- paste0(roundFiveUp(100 * totalSuppressedRegional / totalCellsRegional, 2), "%")

secondarySuppressedRegional <- totalSuppressedRegional - primarySuppressedRegional

secondarySuppressedRegionalPercentage <- paste0(roundFiveUp(100 * secondarySuppressedRegional / totalCellsRegional, 2), "%")

## National suppression statistics ========================================================================================================================================================

primaryNational <- qaFile %>%
  select(-c(
    raw_number_of_leavers, secondary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "National")

totalNational <- qaFile %>%
  select(-c(
    raw_number_of_leavers, primary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
    primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
  )) %>%
  filter(geographic_level == "National")

totalCellsNational <- primaryNational %>% nrow()

primarySuppressedNational <- primaryNational %>%
  filter(primary_number_of_leavers == "c") %>%
  nrow()

primarySuppressedNationalPercentage <- paste0(roundFiveUp(100 * primarySuppressedNational / totalCellsNational, 2), "%")

totalSuppressedNational <- totalNational %>%
  filter(secondary_number_of_leavers == "c") %>%
  nrow()

totalSuppressedNationalPercentage <- paste0(roundFiveUp(100 * totalSuppressedNational / totalCellsNational, 2), "%")

secondarySuppressedNational <- totalSuppressedNational - primarySuppressedNational

secondarySuppressedNationalPercentage <- paste0(roundFiveUp(100 * secondarySuppressedNational / totalCellsNational, 2), "%")

## Combine the suppression statistics =====================================================================================================================================================

suppressionStatistics <- as.data.table(rbind(
  c("Local authority", "Primary", totalCellsLA, primarySuppressedLA, primarySuppressedLAPercentage),
  c("Local authority", "Secondary", totalCellsLA, secondarySuppressedLA, secondarySuppressedLAPercentage),
  c("Local authority", "Total", totalCellsLA, totalSuppressedLA, totalSuppressedLAPercentage),
  c("Regional", "Primary", totalCellsRegional, primarySuppressedRegional, primarySuppressedRegionalPercentage),
  c("Regional", "Secondary", totalCellsRegional, secondarySuppressedRegional, secondarySuppressedRegionalPercentage),
  c("Regional", "Total", totalCellsRegional, totalSuppressedRegional, totalSuppressedRegionalPercentage),
  c("National", "Primary", totalCellsNational, primarySuppressedNational, primarySuppressedNationalPercentage),
  c("National", "Secondary", totalCellsNational, secondarySuppressedNational, secondarySuppressedNationalPercentage),
  c("National", "Total", totalCellsNational, totalSuppressedNational, totalSuppressedNationalPercentage)
))

names(suppressionStatistics) <- c("Geographic level", "Suppression", "Total cells", "Suppressed cells", "Suppressed %")

knitr::kable(suppressionStatistics)
