# Useful QA functions to reuse across the reports ##################################################

# filterArguments and otherIndicators are used in both indicatorSummaryStatistics and compareNationalAggregations - refactor?

# should add the csnum function, though rename to commaSeparate()

# This function is used in place of round() which rounds 5's down
roundFiveUp <- function(x, n) {
  positiveNegative <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^n
  return(z * positiveNegative)
}

# This function is reliant on having data in a molten format, with a 'geog_code' and 'value' column
get_suppression_stats <- function(data) {
  output <- list(
    "total_cells" = nrow(data),
    "suppressed_cells" = sum(grepl("c", data$value)),
    "suppressed_percent" = roundFiveUp(sum(grepl("c", data$value)) / nrow(data) * 100, 2),
    "unique_locations" = length(unique(data$geog_code))
  )
  return(output)
}

showFileSize <- function(data) {
  fileSize <- file.info(data)$size
  
  if (is.null(fileSize)) {
    return(message("Error - no file"))
  } else {
    if (round(fileSize / 1024 / 1024 / 1024, 2) >= 1) {
      return(paste0(roundFiveUp(fileSize / 1024 / 1024 / 1024, 2), " GB"))
    } else {
      if (round(fileSize / 1024 / 1024, 2) < 1) {
        return(paste0(roundFiveUp(fileSize / 1024, 2), " Bytes"))
      } else {
        return(paste0(roundFiveUp(fileSize / 1024 / 1024, 2), " MB"))
      }
    }
  }
}

metadataCrosscheck <- function(data, meta) {
  columnCrosscheck <- function(i) {
    if ((i %in% names(data)) == FALSE) {
      return("FAIL")
    } else {
      return("PASS")
    }
  }
  
  filterGroups <- meta %>%
    filter(filter_grouping_column != "") %>%
    pull(filter_grouping_column)
  
  preResult <- stack(sapply(c(meta$col_name, filterGroups), columnCrosscheck))
  
  missingVariables <- filter(preResult, values == "FAIL") %>% pull(ind)
  
  if ("FAIL" %in% preResult$values) {
    return(message(paste0("The following variables were found in the metadata file, but could not be found in the data file: '", paste(missingVariables, collapse = "', '"), "'.")))
  } else {
    return(message("All variables from the metadata were found in the data file."))
  }
}

# Need to refactor the stages, make it flow better and should make it more descriptive
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

showFilterLevels <- function(data, meta) {
  filters <- meta %>%
    filter(col_type == "Filter") %>%
    pull(col_name)
  
  levelsTable <- function(filter) {
    return(eval(parse(text = paste0("data %>% select(", filter, ") %>% distinct()"))))
  }
  
  output <- lapply(filters, levelsTable)
  
  return(output)
}

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
  
  nationalStage1.5 <- eval(parse(text = paste0("nationalStage1 %>% filter(",indicator," != 'c')")))
  
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

---
  author: "Cam Race"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output: html_document
---
  
  ```{r echo=FALSE}
################################################################################################################
## In theory this is the only section that will vary by report === ALSO ONE OTHER SECTION LATER ON =============
################################################################################################################
## Main files --------------------------------------------------------------------------------------------------

dataPath <- suitabilityData$path
dataFile <- suitabilityData$datafile
qaFile <- suitabilityData$qafile
metaFile <- suitabilityData$meta

## Published file ----------------------------------------------------------------------------------------------
# Put file into a molten format as is easy to get to from both old and new data

published <- suitabilityData$reference %>%
  .[, c(2, 6:38)] %>%
  pivot_longer(-New_geog_code,
               names_to = c("acc_suit", "age_end", "measure"),
               names_pattern = "CL_Acc_(.*)_(.*)_(.*)",
               values_to = "value"
  ) %>%
  mutate(
    age_end = if_else(age_end == "19to21", "19-21", age_end),
    acc_suit = case_when(
      acc_suit == "All" ~ "Total",
      acc_suit == "TotalSUIT" ~ "Suitability total",
      acc_suit == "SUIT" ~ "Suitable",
      acc_suit == "NotSUIT" ~ "Not suitable",
      acc_suit == "NoSUITInfo" ~ "No suitability information available"
    ),
    measure = case_when(
      measure == "" ~ "number_of_leavers",
      measure == "pc" ~ "percentage_of_leavers"
    )
  ) %>%
  filter(age_end == "19-21") %>%
  rename(geog_code = New_geog_code) %>%
  rename(accomodation_suitability = acc_suit)


## Snapshot test file -------------------------------------------------------------------------------------------
# Put file into a molten format as is easy to get to from both old and new data

snapshot <- suitabilityData$snapshot %>%
  mutate(geog_code = case_when(
    geographic_level == "National" ~ country_code,
    geographic_level == "Regional" ~ region_code,
    geographic_level == "Local authority" ~ new_la_code
  )) %>%
  select(time_period, geog_code, accomodation_suitability, age_end, secondary_number_of_leavers, secondary_percentage_of_leavers) %>%
  rename(number_of_leavers = secondary_number_of_leavers, percentage_of_leavers = secondary_percentage_of_leavers) %>%
  pivot_longer(-c(time_period, geog_code, age_end, accomodation_suitability), names_to = "measure", values_to = "value")
```

```{r echo=FALSE}
reportTitle <- paste(basename(dataPath), "QA report")
```

---
  title: `r reportTitle`
---
  
  <ul class="nav nav-pills nav-justified">
  <li class="active"><a data-toggle="pill" href="#file-summary">File summary</a></li>
  <li><a data-toggle="pill" href="#sense-checking">Sense checking</a></li>
  <li><a data-toggle="pill" href="#geography-aggregations">Geography aggregations</a></li>
  <li><a data-toggle="pill" href="#suppression-statistics">Suppression statistics</a></li>
  <li><a data-toggle="pill" href="#2019-publication-comparison">2019 publication comparison</a></li>
  </ul>
  
  
  <div class="tab-content">
  <div id="file-summary" class="tab-pane fade in active">
  
  ---
  
  File name: `r basename(dataPath)`

File size: `r showFileSize(here::here(dataPath))`

Number of rows: `r nrow(dataFile)`

Number of columns: `r ncol(dataFile)`

Number of data cells: `r nrow(dataFile)*nrow(metaFile %>% filter(col_type == "Indicator"))`

---
  
  <details>
  <summary>Variables present</summary>
  `r knitr::kable(names(dataFile), col.names = "", "simple")`
</details>
  
  ---
  
  <details>
  <summary>Metadata file</summary>
  
  `r knitr::kable(metaFile)`

</details>
  
  ---
  
  </div>
  
  <div id="sense-checking" class="tab-pane fade">
  
  ---
  
  <strong>Contents of file</strong>
  
  ---
  
  <details>
  <summary>Time values</summary>
  
  ```{r echo=FALSE}
yearsPresent <- distinct(dataFile %>% select(time_period, time_identifier))

knitr::kable(yearsPresent)
```

</details>
  
  <details>
  <summary>Geography</summary>
  
  ```{r echo=FALSE}
countriesPresent <- distinct(dataFile %>% filter(geographic_level == "National") %>% select(time_period, country_name, country_code)) %>%
  arrange(desc(time_period), country_code)
regionsPresent <- distinct(dataFile %>% filter(geographic_level == "Regional") %>% select(time_period, region_name, region_code)) %>%
  arrange(desc(time_period), region_code)
lasPresent <- distinct(dataFile %>% filter(geographic_level == "Local authority") %>% select(time_period, la_name, old_la_code, new_la_code)) %>%
  arrange(desc(time_period), old_la_code)
```

<details>
  <summary>Count of unique locations by year</summary>
  
  ```{r echo=FALSE}
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
```

</details>
  
  </summary>
  
  <details>
  <summary>Countries present (yearly average: `r nrow(countriesPresent)/nrow(yearsPresent)`):</summary>
  
  `r knitr::kable(countriesPresent, rownames = FALSE)`

</details>
  
  <details>
  <summary>Regions present (yearly average: `r nrow(regionsPresent)/nrow(yearsPresent)`):</summary>
  
  `r knitr::kable(regionsPresent, rownames = FALSE)`

</details>
  
  <details>
  <summary>Local authorities present (yearly average: `r nrow(lasPresent)/nrow(yearsPresent)`):</summary>
  
  `r knitr::kable(lasPresent, rownames = FALSE)`

</details>
  
  </details>
  
  ---
  
  <details>
  <summary>Filters and Indicators</summary>
  
  ---  
  
  Filters present: `r metaFile %>% filter(col_type == "Filter") %>% pull(col_name)`

Indicators present: `r metaFile %>% filter(col_type == "Indicator") %>% pull(col_name)`

---
  
  <details>
  <summary>Filter levels</summary>
  
  ```{r echo=FALSE, comment=NA}

levelsTablesList <- showFilterLevels(dataFile, metaFile)

sapply(levelsTablesList, knitr::kable)
```

</details>
  
  </details>
  
  ---
  
  <strong>Specific snapshots</strong>
  
  ---
  
  <details>
  <summary>Isle of Scilly</summary>
  
  `r knitr::kable(dataFile %>% filter(new_la_code == "E06000053") %>% select(-c("time_identifier", "geographic_level", "country_code", "country_name", "region_name")), rownames = FALSE)`

</details>
  
  <details>
  <summary>City of London</summary>
  
  `r knitr::kable(dataFile %>% filter(old_la_code == 201) %>% select(-c("time_identifier", "geographic_level", "country_code", "country_name", "region_name")), rownames = FALSE)`

</details>
  
  ---
  
  <strong>Cross referencing files</strong>
  
  ---
  
  </details>
  
  <details>
  <summary>Matching to metadata</summary>
  
  ---
  
  ```{r echo=FALSE, comment=NA} 
metadataCrosscheck(dataFile, metaFile)
```

---
  
  </details>
  
  <details>
  <summary>Matching to QA version</summary>
  
  Comparing the files after the QA columns are stripped from the QA version

```{r echo=FALSE, comment=NA}
waldo::compare(
  dataFile,
  qaFile %>%
    select(-c(
      raw_number_of_leavers, primary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers,
      primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
    )) %>%
    rename(number_of_leavers = secondary_number_of_leavers, percentage_of_leavers = secondary_percentage_of_leavers)
)
```

</details>
  
  ---
  
  <strong>Summary statistics</strong>
  
  ---
  
  <details>
  <summary>Number of leavers against the filters by year (National)</summary>
  
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(
  indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "National"), metaFile, "number_of_leavers") %>%
    select(-c("min", "max", "q1", "median", "q3")) %>%
    pivot_wider(names_from = time_period, values_from = mean)
))
```

</details>  
  
  <details>
  <summary>Number of leavers against the filters by year (Regional)</summary>
  
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "Regional"), metaFile, "number_of_leavers")))
```

</details>  
  
  <details>
  <summary>Number of leavers against the filters by year (Local authority)</summary>
  
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "Local authority"), metaFile, "number_of_leavers")))
```

</details>  
  
  <details>
  <summary>Percentage of leavers against the filters by year (National)</summary>
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "National"), metaFile, "percentage_of_leavers") %>%
                                select(-c("min", "max", "q1", "median", "q3")) %>%
                                pivot_wider(names_from = time_period, values_from = mean)))
```

</details>
  
  <details>
  <summary>Percentage of leavers against the filters by year (Regional)</summary>
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "Regional"), metaFile, "percentage_of_leavers")))
```

</details>
  
  <details>
  <summary>Percentage of leavers against the filters by year (Local authority)</summary>
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}

suppressWarnings(knitr::kable(indicatorSummaryStatistics(dataFile %>% filter(geographic_level == "Local authority"), metaFile, "percentage_of_leavers")))
```

</details>
  
  ---
  
  </div>
  
  <div id="geography-aggregations" class="tab-pane fade">
  
  This section uses the raw unrounded data from the QA version of the file for regional and local authority aggregations, and then checks if rounding to the nearest 10 will match the national aggregations from the final version of the file. 

It is assumed that the QA and final files match up - see <i>Matching to QA version</i> in the [sense checking](#sense-checking) section.
  
  This focusses purely on the <i>number_of_leavers</i> indicator.
  
  ```{r echo=FALSE, warning=FALSE, message=FALSE}
  geographyComparisonTable <- compareNationalAggregations(dataFile, qaFile, metaFile, "number_of_leavers")
  
  totalAggregations <- nrow(geographyComparisonTable) * 3
  
  overallPercentageMatch <- paste0(roundFiveUp(100 * sum(geographyComparisonTable$regionalMatchNational, geographyComparisonTable$laMatchNational, geographyComparisonTable$laMatchRegional)
                                               / totalAggregations, 2), "%")
  
  regionalMatchNational <- sum(geographyComparisonTable$regionalMatchNational)
  
  laMatchNational <- sum(geographyComparisonTable$laMatchNational)
  
  laMatchRegional <- sum(geographyComparisonTable$laMatchRegional)
  ```
  
  ---
    
    `r overallPercentageMatch` of all `r totalAggregations` aggregations match up.
  
  `r regionalMatchNational` of `r nrow(geographyComparisonTable)` regional aggregations match up to the national totals.
  
  `r laMatchNational` of `r nrow(geographyComparisonTable)` local authority aggregations match up to the national totals.
  
  `r laMatchRegional` of `r nrow(geographyComparisonTable)` local authority aggregations match up to the regional totals.
  
  ---
    
    <details>
    <summary>View full table</summary>
    
    `r knitr::kable(geographyComparisonTable)`
  
  </details>
    
    
    ---
    
    </div>
    
    <div id="suppression-statistics" class="tab-pane fade">
    
    ---
    
    Suppression statistics have been calculated on the <i>number_of_leavers</i> variable, as the <i>percentage_of_leavers</i> variable simply mirrors its suppression.
  
  ---
    
    There are `r  qaFile$total_row_count[1]` cells of data in the final file, of these, `r qaFile$total_suppressed_count[1]` (`r paste0(roundFiveUp(100*qaFile$total_suppressed_count[1]/qaFile$total_row_count[1],2),"%")`) are suppressed.
  
  ---
    
    <details>
    <summary>Primary and secondary suppression breakdown</summary>
    
    ```{r echo=FALSE}
  
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
  
  secondarySuppressedLAPercentage <- paste0(roundFiveUp(100*secondarySuppressedLA / totalCellsLA, 2), "%")
  
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
  
  secondarySuppressedRegionalPercentage <- paste0(roundFiveUp(100*secondarySuppressedRegional / totalCellsRegional, 2), "%")
  
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
  
  secondarySuppressedNationalPercentage <- paste0(roundFiveUp(100*secondarySuppressedNational / totalCellsNational, 2), "%")
  
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
  ```
  
  </details>
    
    ---
    
    <details>
    <summary>Suppression by filters and year (local authority level)</summary>
    
    ```{r suppressionOverview, echo=FALSE, warning=FALSE, message=FALSE}
  suppressionOverviewStage1 <- qaFile %>%
    select(-c(
      raw_number_of_leavers, primary_number_of_leavers, numerator, pre_denominator, denominator, raw_percentage_of_leavers, primary_percentage_of_leavers, secondary_percentage_of_leavers,
      primary_suppressed_count, secondary_suppressed_count, total_suppressed_count, total_row_count
    ))
  
  filterArguments <- paste0(metaFile %>% filter(col_type == "Filter") %>% pull(col_name), collapse = ", ")
  
  suppressionOverviewStage2 <- eval(parse(text= paste0("suppressionOverviewStage1 %>% group_by(time_period,",filterArguments,")"))) %>% 
    summarise(
      "total_cells" = length(secondary_number_of_leavers),
      "suppressed_cells" = sum(grepl("c", secondary_number_of_leavers)),
      "suppressed_percent" = paste0(roundFiveUp(sum(grepl("c", secondary_number_of_leavers)) * 100 / length(secondary_number_of_leavers), 2), "%")
    ) %>%
    arrange(desc(time_period))
  
  suppressWarnings(knitr::kable(suppressionOverviewStage2))
  ```
  
  </details>
    
    ---
    
    </div>
    
    <div id="2019-publication-comparison" class="tab-pane fade">
    
    ---
    
    This section of the QA takes data from the published file on ages 19-21 from 2019 and compares that to the 2019 data in the database ...1023 using the code that is being worked on for the 2020 publication.
  
  ```{r include=FALSE}
  ################################################################################################################
  ## NEEDS FUDGERY ===============================================================================================
  ################################################################################################################
  # Get the suppression summary numbers for the QA ----------------------------------------------------------------
  
  published_summary <- get_suppression_stats(published)
  snapshot_summary <- get_suppression_stats(snapshot %>% filter(time_period == 2019) %>% filter(age_end == "19-21"))
  
  # Join the snapshot and published files together ---------------------------------------------------------------------------------
  
  joined <- snapshot %>%
    filter(time_period == 2019) %>%
    filter(age_end == "19-21") %>%
    select(-c(time_period)) %>%
    full_join(published, by = c("geog_code" = "geog_code", "accomodation_suitability" = "accomodation_suitability", "age_end" = "age_end", "measure" = "measure"), suffix = c(".snapshot", ".published")) %>%
    mutate(matching = if_else(value.published == value.snapshot, TRUE, FALSE)) %>%
    subset(!(measure == "percentage_of_leavers" & accomodation_suitability %in% c("Total", "Suitability total")))
  ```
  
  ---
    
    <strong>Suppression comparison</strong>
    
    ---
    
    <details>
    <summary>Suppression summary</summary>
    
    |                  | published in 2019                         | snapshot file using 2020 code            |
    |------------------|-------------------------------------------|------------------------------------------|
    | Total cells      | `r published_summary$total_cells`         | `r snapshot_summary$total_cells`         |
    | Suppressed cells | `r published_summary$suppressed_cells`    | `r snapshot_summary$suppressed_cells`    |
    | Suppressed %     | `r published_summary$suppressed_percent`% | `r snapshot_summary$suppressed_percent`% |
    
    </details>
    
    <details>
    <summary>Under suppression</summary>
    
    `r joined %>% filter(value.snapshot != "c" & value.published == "c") %>% nrow()` cells were suppressed in the 2019 published file that have not been suppressed in the 2020 file.
  
  ```{r echo=FALSE, comment=NA}
  
  underSuppression <- joined %>% filter(value.snapshot != "c" & value.published == "c")
  
  if (nrow(underSuppression) > 0) {
    knitr::kable(underSuppression)
  } else {
    message("Congrats - no under suppression!")
  }
  ```
  
  </details>
    
    <details>
    <summary>Over suppression</summary>
    
    `r joined %>% filter(value.snapshot == "c" & value.published != "c") %>% nrow()` cells are suppressed in the 2020 file that were not suppressed in the 2019 published file.
  
  ```{r echo=FALSE, comment=NA}
  overSuppression <- joined %>% filter(value.snapshot == "c" & value.published != "c")
  
  if (nrow(overSuppression) > 0) {
    knitr::kable(overSuppression)
  } else {
    message("Congrats - no over suppression!")
  }
  ```
  
  </details>
    
    ---
    
    <strong>General comparison</strong>
    
    Of `r nrow(joined)` cells, `r sum(joined$matching)` directly match the values of the published figures from 2019 (`r roundFiveUp(sum(joined$matching)/nrow(joined)*100,2)`%).
  
  For this section, cells related to percentages for the Suitability total and the full Total have been omitted as they were not present in the original file (and are represented by no data symbols or 100 in this year's file). This is a result of the change in structure of the final data.

---

<details>
  <summary>Snapshot matching</summary>

Snapshot: England, North East and Darlington for 2019

```{r suitabilitySnapshot, echo=FALSE}

knitr::kable(joined %>% filter(geog_code %in% c("E92000001", "E12000001", "E06000005")))
```

</details>

<details>
  <summary>Cell by cell matching</summary>

There were `r nrow(joined %>% filter(matching == FALSE))` cells that didn't match
                                                                                                                                                               
                                                                                                                                                               ```{r notMatching, echo=FALSE, comment=NA}
                                                                                                                                                               if (nrow(joined %>% filter(matching == FALSE)) > 0) {
                                                                                                                                                                 knitr::kable(joined %>% filter(matching == FALSE))
                                                                                                                                                               } else {
                                                                                                                                                                 message("Congrats - it all matches")
                                                                                                                                                               }
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               </details>
                                                                                                                                                                 
                                                                                                                                                                 <details>
                                                                                                                                                                 <summary>Cells present in snapshot file but not published file</summary>
                                                                                                                                                                 
                                                                                                                                                                 ```{r snapshotNotPublished, echo=FALSE, comment=NA}
                                                                                                                                                               if (nrow(joined %>% filter(is.na(value.published))) > 0) {
                                                                                                                                                                 knitr::kable(joined %>% filter(is.na(value.published)))
                                                                                                                                                               } else {
                                                                                                                                                                 message("Well done - no cells were in the snapshot file that weren't in the published file")
                                                                                                                                                               }
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               </details>
                                                                                                                                                                 
                                                                                                                                                                 <details>
                                                                                                                                                                 <summary>Cells present in published file but not snapshot</summary>
                                                                                                                                                                 
                                                                                                                                                                 ```{r publishedNotsnapshot, echo=FALSE, comment=NA}
                                                                                                                                                               if (nrow(joined %>% filter(is.na(value.snapshot))) > 0) {
                                                                                                                                                                 knitr::kable(joined %>% filter(is.na(value.snapshot)))
                                                                                                                                                               } else {
                                                                                                                                                                 message("Well done - no cells were missing from the snapshot file that were in the published file")
                                                                                                                                                               }
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               </details>
                                                                                                                                                                 
                                                                                                                                                                 ---
                                                                                                                                                                 
                                                                                                                                                                 </div>
                                                                                                                                                                 </div>
                                                                                                                                                                 
