library(tidyverse)

dat_seds_presidential_incumbent = read_csv("cleaned_data/merged_datasets/seds_pres_inc_data.csv") %>% 
    filter(year != 2024)


# ---------- Main Model (->2012 and ->2016) ----------

dat_fd_pres_inc = dat_seds_presidential_incumbent %>%
    filter(year != 2024) %>%  # data gap
    arrange(county_fips, year) %>%
    group_by(county_fips) %>%
    mutate(
        lag_incumbent_party = lag(incumbent_party),
        same_party_incumbent = (incumbent_party == lag_incumbent_party),
        d_inc_vote = case_when(
        same_party_incumbent == TRUE ~ 
            incumbent_vote_pct - lag(incumbent_vote_pct),
        same_party_incumbent == FALSE ~ 
            incumbent_vote_pct - (1 - lag(incumbent_vote_pct)),
        TRUE ~ NA_real_
        ),
        
        # Treatment
        d_gas = gas_price - lag(gas_price),
        
        d_unemp = percent_unemployment - lag(percent_unemployment),
        d_income = median_income_adj2024 - lag(median_income_adj2024),
        d_lfpr = labor_force_participation - lag(labor_force_participation),
        d_age = median_age - lag(median_age),
        d_edu = percent_upper_edu - lag(percent_upper_edu),
        d_white = percent_white - lag(percent_white),
        d_black = percent_black - lag(percent_black),
        d_asian = percent_asian - lag(percent_asian),
        d_native = percent_native - lag(percent_native),
        d_manufacturing = percent_manufacturing - lag(percent_manufacturing),
        d_edu_health = percent_edu_health_services - lag(percent_edu_health_services),
        d_homeowner = percent_homeowner - lag(percent_homeowner),
        d_home_value = median_home_value_adj2024 - lag(median_home_value_adj2024),
        d_family_hh = percent_family_households - lag(percent_family_households),
        d_married = percent_married - lag(percent_married),
        d_hh_size = avg_household_size - lag(avg_household_size),
        d_commute = mean_commute_time - lag(mean_commute_time),
        d_pop = total_population - lag(total_population),
        d_pop_density = pop_density_sq_mile - lag(pop_density_sq_mile),
        d_percent_car = percent_car - lag(percent_car),
        d_percent_pt = percent_pt - lag(percent_pt),
        d_partisan_lean = partisan_lean - lag(partisan_lean)) %>% 
    ungroup() %>%
    filter(!is.na(d_gas), !is.na(d_inc_vote)) %>% 
    select(state,
    year, county_fips, d_inc_vote, d_gas, d_unemp,
    d_income, d_lfpr, d_age, d_edu, d_white,
    d_black, d_asian, d_native, d_manufacturing, d_edu_health,
    d_homeowner, d_home_value, d_family_hh, d_married, d_hh_size,
    d_commute, d_pop, d_pop_density, d_percent_car, d_percent_pt,

    incumbent_party, lag_incumbent_party, 
            same_party_incumbent, incumbent_vote_pct, d_inc_vote, d_partisan_lean
    )


write_csv(dat_fd_pres_inc, "cleaned_data/merged_datasets/fd_pres_inc.csv")


# ---------- Lag Placebo Data) ----------
dat_fd_pres_with_lag = dat_fd_pres_inc %>%
  arrange(county_fips, year) %>%
  group_by(county_fips) %>%
  mutate(
    d_gas_lag = lag(d_gas),
    d_percent_pt_lag = lag(d_percent_pt)) %>% 
  filter(year != 2012) # get rid of the false d_gas_future 


write_csv(dat_fd_pres_with_lag, "cleaned_data/merged_datasets/fd_pres_inc_lag_placebo.csv")


# ---------- Future Placebo Data  (->2016) ----------

dat_fd_pres_with_future = dat_fd_pres_inc %>%
  arrange(county_fips, year) %>%
  group_by(county_fips) %>%
  mutate(
    d_gas_future = lead(d_gas),
    d_percent_pt_future = lead(d_percent_pt)
  ) %>%
  filter(!is.na(d_inc_vote), !is.na(d_gas_future)) %>% 
  filter(year != 2016) # get rid of the false d_gas_future 


write_csv(dat_fd_pres_with_future, "cleaned_data/merged_datasets/fd_pres_inc_future_placebo.csv")
