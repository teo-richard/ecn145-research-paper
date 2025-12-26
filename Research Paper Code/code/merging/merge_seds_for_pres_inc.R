library(tidyverse)
library(readr)
library(readxl)

# Note on SEDS analysis stuff: the SEDS dataste has all 50 states but is only annual averages and missing 2024.
# In the seds cleaning script, we added annual averages for each state for 2024 using AAA data. 

voter_data = read_csv("cleaned_data/voter_datasets/voter_data_pres_inc.csv") %>%
  mutate(partisan_lean = dem_vote_pct - rep_vote_pct)
seds_gas = read_csv("cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")

commute_data = read_csv("cleaned_data/general_datasets/cleaned_commute_data.csv")
control_data = read_csv("cleaned_data/general_datasets/cleaned_controls.csv")


voter_data = voter_data %>% filter(year %in% c(2000, 2002, 2004, 2008, 2010, 2012, 2016, 2024))
voter_data$county_fips = as.character(voter_data$county_fips)


x = inner_join(voter_data, commute_data %>% select(-NAME), by = join_by(county_fips == GEOID, year == year))
y = inner_join(x, control_data, by = join_by(county_fips == GEOID, year == year)) %>% dplyr::select(-any_of("...1"))


# Create a state abbreviation column from STCOFIPS10
state_fips_lookup = c(
  "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
  "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
  "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
  "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
  "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
  "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
  "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
  "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
  "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
  "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
  "56" = "WY"
)


y = y %>%
    mutate(state = state_fips_lookup[str_sub(county_fips, 1, 2)], .after = county_fips)


z = inner_join(y, seds_gas, by = join_by(state == state, year == year))



write_csv(z, "cleaned_data/merged_datasets/seds_pres_inc_data.csv")

