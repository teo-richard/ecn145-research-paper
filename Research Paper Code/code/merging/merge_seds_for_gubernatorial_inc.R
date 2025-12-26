library(tidyverse)

seds_gas = read_csv("cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")
guber_data = read_csv("cleaned_data/voter_datasets/guber_voter_data.csv") %>% 
    mutate(partisan_lean = dem_vote_pct - rep_vote_pct)
# 425

commute_data = read_csv("cleaned_data/general_datasets/cleaned_commute_data.csv")
control_data = read_csv("cleaned_data/general_datasets/cleaned_controls.csv")


x = inner_join(guber_data, commute_data %>% select(-NAME), by = join_by(county_fips == GEOID, year == year))
y = inner_join(x, control_data, by = join_by(county_fips == GEOID, year == year)) %>% dplyr::select(-any_of("...1"))
z = inner_join(y, seds_gas, by = join_by(state == state, year == year))

write_csv(z, "cleaned_data/merged_datasets/seds_gubernatorial_inc_data.csv")


u_st = function(data) {
  return(length(unique(data$STCOFIPS10)))
}
u_cf = function(data) {
  return(length(unique(data$county_fips)))
}
u_gi = function(data) {
  return(length(unique(data$GEOID)))
}
u_st(seds_gas)
u_cf(guber_data)
u_gi(seds_gas)

