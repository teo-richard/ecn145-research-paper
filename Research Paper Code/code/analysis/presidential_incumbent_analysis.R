library(tidyverse)
library(readr)
library(lmtest)
library(sandwich)

dat_pres_inc = read_csv("cleaned_data/merged_datasets/seds_pres_inc_data.csv") %>% 
  select(-percent_manufacturing, -percent_edu_health_services, -percent_car)

dat_fd_pres_inc = read_csv("cleaned_data/merged_datasets/fd_pres_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

eia_dat_fd_pres_inc = read_csv("cleaned_data/merged_datasets/eia_fd_pres_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

dat_fd_pres_inc_future = read_csv("cleaned_data/merged_datasets/fd_pres_inc_future_placebo.csv")
eia_dat_fd_pres_inc_future = read_csv("cleaned_data/merged_datasets/eia_fd_pres_inc_future_placebo.csv")

dat_fd_pres_inc_lag = read_csv("cleaned_data/merged_datasets/fd_pres_inc_lag_placebo.csv")
eia_dat_fd_pres_inc_lag = read_csv("cleaned_data/merged_datasets/eia_fd_pres_inc_lag_placebo.csv")

# :::::::::::::::::::::::::::::::::::::::: FIRST DIFFERENCES ::::::::::::::::::::::::::::::::::::::::
# -----  EIA -----
eia_model_fd_inc_main_pres = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
    data = eia_dat_fd_pres_inc)

summary(eia_model_fd_inc_main_pres)
  
# -------- FD MAIN --------
model_fd_inc_main_pres = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
    data = dat_fd_pres_inc)
    

summary(model_fd_inc_main_pres)

texreg(model_dat_fd_pres_inc_lag,
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("d_gas_lag" = "Lagged Gas Price"),
       caption.above=TRUE,
       float = "H")


# -------- FD Placebo: Future --------
# Fails
model_dat_fd_pres_inc_future = feols(
  d_inc_vote ~ d_gas_future + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_pres_inc_future
)

summary(model_dat_fd_pres_inc_future)

cor(dat_fd_pres_inc_future$d_gas, dat_fd_pres_inc_future$d_gas_future) # 0.2030707


eia_model_dat_fd_pres_inc_future = feols(
  d_inc_vote ~ d_gas_future + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = eia_dat_fd_pres_inc_future
)

summary(eia_model_dat_fd_pres_inc_future)


# -------- FD Placebo: Past --------
# Fails
model_dat_fd_pres_inc_lag = feols(
  d_inc_vote ~ d_gas_lag + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_pres_inc_lag
)

summary(model_dat_fd_pres_inc_lag)


cor(dat_fd_pres_inc_lag$d_gas, dat_fd_pres_inc_lag$d_gas_lag, use = "complete.obs") # 0.203

eia_model_dat_fd_pres_inc_lag = feols(
  d_inc_vote ~ d_gas_lag + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = eia_dat_fd_pres_inc_lag
)

summary(eia_model_dat_fd_pres_inc_lag)

cor(eia_dat_fd_pres_inc_lag$d_gas, eia_dat_fd_pres_inc_lag$d_gas_lag, use = "complete.obs") # -0.2734054


# ~~~~~~~~~~ Horse Race for FD Placebo: Past ~~~~~~~~~~

model_pres_inc_future_hr = feols(
  d_inc_vote ~ d_gas + d_gas_future + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_pres_inc_future
)

summary(model_pres_inc_future_hr)

model_pres_inc_lag_hr = feols(
  d_inc_vote ~ d_gas + d_gas_lag + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_pres_inc_lag
)

summary(model_pres_inc_lag_hr)


eia_model_pres_inc_lag_hr = feols(
  d_inc_vote ~ d_gas + d_gas_lag + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = eia_dat_fd_pres_inc_lag
)

summary(eia_model_pres_inc_lag_hr)

# :::::::::::::::::::::::::::::::::::::::: LEVEL ::::::::::::::::::::::::::::::::::::::::
# -------- Level model --------


model_level_inc_pres = lm(incumbent_vote_pct ~ gas_price + percent_pt +
                         percent_unemployment + median_income_adj2024 + 
                         median_age + percent_upper_edu + 
                         percent_white + mean_commute_time + 
                         total_population + 
                         factor(state) + factor(year),
                         data = dat_pres_inc)

coeftest(model_level_inc_pres, vcov = vcovCL, cluster = ~county_fips)[
  c("gas_price"), 
]
summary(model_level_inc_pres)
# -------- Level Placebo: Future --------

dat_placebo_lead_inc_pres = dat_presidential_incumbent %>% 
  arrange(county_fips, year) %>% 
  group_by(county_fips) %>% 
  mutate(gas_lead = lead(gas_price, 1)) %>% 
  filter(!is.na(gas_lead))



model_placebo_lead_inc_pres = lm(incumbent_vote_pct ~ gas_lead + 
                         percent_unemployment + median_income_adj2024 + 
                         median_age + percent_upper_edu + 
                         percent_white + mean_commute_time + 
                         total_population +
                         factor(state) + factor(year),
                         data = dat_placebo_lead_inc_pres)

coeftest(model_placebo_lead_inc_pres, vcov = vcovCL, cluster = ~county_fips)[
  c("gas_lead"), 
]

# -------- Level Placebo: Past --------

dat_placebo_lag_inc_pres = dat_seds_incdat_presidential_incumbentumbent %>% 
  arrange(county_fips, year) %>% 
  group_by(county_fips) %>% 
  mutate(gas_lag = lag(gas_price, 1)) %>% 
  filter(!is.na(gas_lag))



model_placebo_lag_inc_pres = lm(incumbent_vote_pct ~ gas_lag + 
                         percent_unemployment + median_income_adj2024 + 
                         median_age + percent_upper_edu + 
                         percent_white + mean_commute_time + 
                         total_population +
                         factor(state) + factor(year),
                         data = dat_placebo_lag_inc_pres)

coeftest(model_placebo_lag_inc_pres, vcov = vcovCL, cluster = ~county_fips)[
  c("gas_lag"), 
]


