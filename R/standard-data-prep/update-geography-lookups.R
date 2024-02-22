library(readr)
# @param open_geography_file Lookup file sourced from 
#       https://geoportal.statistics.gov.uk/search?q=LUP_WPC&sort=Date%20Created%7Ccreated%7Cdesc
#       e.g. data/downloaded_source_data/Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_to_Upper_Tier_L.csv
update_pcon_la_lookup <- function(open_geography_file, existing_data_file = 'data/la_pcon_hierarchy.csv'){
    new_data <- read_csv(open_geography_file)
    new_year <- names(new_data) %>% 
      as.data.frame() %>% 
      filter(grepl("PCON",.), grepl("CD",.)) %>% 
      pull(.) %>% 
      gsub("PCON","",.) %>% 
      gsub("CD","",.)
    new_lookup <- new_data %>% 
      select(
        pcon_code = paste0("PCON",new_year,"CD"),
        pcon_name = paste0("PCON",new_year,"NM"),
        new_la_code = paste0("UTLA",new_year,"CD"),
        la_name = paste0("UTLA",new_year,"NM"),
      ) %>% 
      distinct() %>%
      mutate(
        first_available_year_active = paste0("20",new_year),
        most_recent_year_active = paste0("20",new_year)
      )
    if(!is.null(existing_data_file)){
      message("Reading data from existing file: ", existing_data_file)
      existing_lookup <- read_csv(existing_data_file)
      new_lookup <- existing_lookup %>%
        rbind(new_lookup) %>%
        summarise(
          first_available_year_active = min(first_available_year_active), 
          most_recent_year_active = max(most_recent_year_active), 
          .by=c("pcon_code", "pcon_name", "new_la_code", "la_name"))
    }
    new_lookup %>% arrange(pcon_code, new_la_code) %>% write.csv("data/la_pcon_hierarchy.csv", row.names=FALSE)
}
