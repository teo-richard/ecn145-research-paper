dat_seds_turnout = read_csv("cleaned_data/merged_datasets/seds_turnout_data.csv")

# In your LEVELS data (before first-differencing)
summary(dat_seds_turnout$VOTER_TURNOUT_PCT)

# Find all negative turnout values
dat_seds_turnout %>%
  filter(VOTER_TURNOUT_PCT < 0) %>%
  select(STCOFIPS10, YEAR, VOTER_TURNOUT_PCT, everything())

dat_seds_turnout = dat_seds_turnout %>%
  mutate(VOTER_TURNOUT_PCT = ifelse(VOTER_TURNOUT_PCT < 0, 
                                     NA, 
                                     VOTER_TURNOUT_PCT))


# ---------- Main Model ----------

dat_fd_turnout_seds = dat_seds_turnout %>%
  arrange(STCOFIPS10, YEAR) %>%
  group_by(STCOFIPS10) %>%
  mutate(
    # Outcome
    d_turnout = VOTER_TURNOUT_PCT - lag(VOTER_TURNOUT_PCT),

    # partisan lean
    d_partisan_index_dem = PARTISAN_INDEX_DEM - lag(PARTISAN_INDEX_DEM),
    
    # Treatment
    d_gas = gas_price - lag(gas_price),
    
    # Existing controls (differenced)
    d_unemp = percent_unemployment - lag(percent_unemployment),
    d_income = median_income_adj2024 - lag(median_income_adj2024),
    d_age = median_age - lag(median_age),
    d_edu = percent_upper_edu - lag(percent_upper_edu),
    d_white = percent_white - lag(percent_white),
    d_commute = mean_commute_time - lag(mean_commute_time),
    d_pop = total_population - lag(total_population),
    d_pop_density = pop_density_sq_mile - lag(pop_density_sq_mile),
    
    # NEW: Race/ethnicity controls (differenced)
    d_black = percent_black - lag(percent_black),
    d_asian = percent_asian - lag(percent_asian),
    d_native = percent_native - lag(percent_native),
    
    # NEW: Economic structure (differenced)
    d_lfpr = labor_force_participation - lag(labor_force_participation),
    d_manufacturing = percent_manufacturing - lag(percent_manufacturing),
    d_edu_health = percent_edu_health_services - lag(percent_edu_health_services),
    d_percent_car = percent_car - lag(percent_car),
    d_percent_pt = percent_pt - lag(percent_pt),
    
    # NEW: Housing (differenced)
    d_homeowner = percent_homeowner - lag(percent_homeowner),
    d_home_value = median_home_value_adj2024 - lag(median_home_value_adj2024),
    
    # NEW: Family structure (differenced)
    d_family_hh = percent_family_households - lag(percent_family_households),
    d_married = percent_married - lag(percent_married),
    d_hh_size = avg_household_size - lag(avg_household_size),

        # CREATE PLACEBO VARIABLES HERE (within same group_by)
    d_gas_lag = lag(d_gas),      # Lagged gas price change
    d_gas_future = lead(d_gas)   # Future gas price change

  ) %>%
  filter(!is.na(d_gas), !is.na(d_turnout))


write_csv(dat_fd_turnout_seds, "cleaned_data/merged_datasets/fd_turnout.csv")




# ---------- Lagged Placebo Data ----------

dat_fd_turnout_lag = dat_fd_turnout_seds %>% 
  arrange(STCOFIPS10, YEAR) %>%
  group_by(STCOFIPS10) %>%
  mutate(
    d_gas_lag = lag(d_gas),
    d_percent_pt_lag = lag(d_percent_pt)) %>% 
  filter(!is.na(d_gas_lag), !is.na(d_turnout)) %>% 
  filter(YEAR != 2012)

write_csv(dat_fd_turnout_lag, "cleaned_data/merged_datasets/fd_turnout_lag.csv")


  # ---------- Lead Placebo Data ----------

dat_fd_turnout_lead = dat_fd_turnout_seds %>% 
  arrange(STCOFIPS10, YEAR) %>%
  group_by(STCOFIPS10) %>%
  mutate(
    d_gas_lead = lead(d_gas),
    d_percent_pt_lead = lead(d_percent_pt)) %>% 
  filter(!is.na(d_gas_lead), !is.na(d_turnout)) %>% 
  filter(YEAR != 2018)

write_csv(dat_fd_turnout_lead, "cleaned_data/merged_datasets/fd_turnout_lead.csv")

