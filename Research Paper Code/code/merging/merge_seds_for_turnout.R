library(tidyverse)
library(readr)

# Note on SEDS analysis stuff: the SEDS dataste has all 50 states but is only annual averages and missing 2024.
# In the seds cleaning script, we added annual averages for each state for 2024 using AAA data. 

seds_gas = read_csv("cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")

voter_data = read_csv("cleaned_data/voter_datasets/general_cleaned_voter_data.csv")
# unique counties = 3115
#guber_data = read_csv("cleaned_data/merged_datasets/seds_gubernatorial_inc_data.csv")
# 425 unique counties

# voter_data_with_partisan = left_join(voter_data, guber_data, by = c("STCOFIPS10" = "county_fips", "YEAR" = "year")) %>% 
  select(names(voter_data), partisan_lean)

commute_data = read_csv("cleaned_data/general_datasets/cleaned_commute_data.csv")
# 631 unique counties
control_data = read_csv("cleaned_data/general_datasets/cleaned_controls.csv")
# 853 unique counties



x = inner_join(voter_data, commute_data %>% select(-NAME), by = join_by(STCOFIPS10 == GEOID, YEAR == year))
# 536 unique counties
y = inner_join(x, control_data, by = join_by(STCOFIPS10 == GEOID, YEAR == year)) %>% dplyr::select(-any_of("...1"))
# 534 unique counties


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
    mutate(state = state_fips_lookup[str_sub(STCOFIPS10, 1, 2)], .after = STCOFIPS10)
z = inner_join(y, seds_gas, by = join_by(state == state, YEAR == year))

write_csv(z, "cleaned_data/merged_datasets/seds_turnout_data.csv")

