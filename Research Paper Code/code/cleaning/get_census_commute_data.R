library(tidyverse)
library(tidycensus)

# Note: This will have data for all 50 states, for only the years in the vector `election_years`

# Data from the ACS, see the table below for where it's from 

census_api_key("693ff38dbd441165b297753e2f7cccfa5b27c7e2")

election_years = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2022, 2024)

dat_all_years <- map_dfr(election_years, function(yr) {
  get_acs(
    geography = "county",
    variables = c("B08301_001", "B08301_002", "B08301_010"),
    # state = st,
    year = yr,
    survey = "acs1",
    cache_table = TRUE
  ) %>% 
    mutate(year = yr)
})

commuter_data = dat_all_years %>% 
    select(-moe) %>% 
    pivot_wider(names_from = "variable", values_from = "estimate") %>% 
    select(GEOID, NAME, year, B08301_001, B08301_002, B08301_010) %>% 
    rename_with(
        ~ c("total", "car", "pt"),
        .cols = starts_with("B")
    ) %>% 
    mutate(
        percent_car = car / total,
        percent_pt = pt / total
    ) %>% 
    select(-total, -car, -pt) %>% 
    drop_na()
    # %>% 
    # filter(str_detect(NAME, "California|Colorado|Florida|Massachusetts|Minnesota|New York|Ohio|Texas|Washington")) %>% 
    # na.omit()


write_csv(commuter_data, "cleaned_data/general_datasets/cleaned_commute_data.csv")


