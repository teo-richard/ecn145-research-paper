library(tidyverse)
library(readr)
library(lmtest)
library(sandwich)
library(car)
library(texreg)
library(fixest)

# We expect coefficient on gas_price to be (+)


dat_turnout = read_csv("cleaned_data/merged_datasets/seds_turnout_data.csv") %>% 
  select(-percent_manufacturing, -percent_edu_health_services, -percent_car)

fd_turnout = read_csv("cleaned_data/merged_datasets/fd_turnout.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

fd_turnout_lead = read_csv("cleaned_data/merged_datasets/fd_turnout_lead.csv") %>% 
  select(-percent_manufacturing, -percent_edu_health_services, -percent_car)

fd_turnout_lag = read_csv("cleaned_data/merged_datasets/fd_turnout_lag.csv") %>% 
  select(-percent_manufacturing, -percent_edu_health_services, -percent_car)


# :::::::::::::::::::::::::::::::::::::::: FIRST DIFFERENCES ::::::::::::::::::::::::::::::::::::::::

# -------- FD MAIN --------
model_fd_turnout = feols(
  d_turnout ~ d_gas + d_percent_pt + d_partisan_index_dem +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + YEAR,
    cluster = ~state,
  data = fd_turnout
)

summary(model_fd_turnout)


texreg(model_fd_turnout,
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("d_gas" = "Gas Price"),
       caption.above=TRUE,
       float = "H")



# -------- FD Placebo: Future --------
model_fd_turnout_lead = feols(
  d_turnout ~ d_gas_lead + percent_pt + d_partisan_index_dem +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + YEAR,
    
  data = fd_turnout_lead
)

summary(model_fd_turnout_lead)


cor(fd_turnout_lead$d_gas, fd_turnout_lead$d_gas_lead) # -0.4001903

model_fd_turnout_lead_main = feols(
  d_turnout ~ d_gas + d_percent_pt + d_partisan_index_dem +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + YEAR,
    cluster = ~state,
  data = fd_turnout_lead
)

summary(model_fd_turnout_lead_main)


# ~~~~~~~~~~ Horse Race ~~~~~~~~~~
model_fd_turnout_lead_hr = feols(
  d_turnout ~ d_gas + d_gas_lead+ percent_pt + d_partisan_index_dem +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + YEAR,
    
  data = fd_turnout_lead
)

summary(model_fd_turnout_lead_hr)

# -------- FD Placebo: Past --------
model_fd_turnout_lag = feols(
  d_turnout ~ d_gas_lag + d_percent_pt + d_partisan_index_dem +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + YEAR,
    
  data = fd_turnout_lag
)

summary(model_fd_turnout_lag)




# ***** Checking Correlation *****

cor(fd_turnout_lead$d_gas, fd_turnout_lead$d_gas_future) # -0.4
cor(fd_turnout_lead$d_percent_pt, fd_turnout_lead$d_percent_pt_lead) # -0.283
cor(fd_turnout_lead$d_gas, fd_turnout_lead$d_percent_pt_lead) # 0.1869

cor(fd_turnout_lag$d_gas, fd_turnout_lag$d_gas_lag) # 0



# :::::::::::::::::::::::::::::::::::::::: LEVEL ::::::::::::::::::::::::::::::::::::::::
# -------- Level model --------

model_level_turnout = lm(
  VOTER_TURNOUT_PCT ~ gas_price + percent_pt +
    percent_unemployment + median_income_adj2024 + labor_force_participation +
    median_age + percent_upper_edu + 
    percent_white + percent_black +
    percent_asian + percent_native +
    percent_homeowner + median_home_value_adj2024 +
    percent_family_households + percent_married + avg_household_size +
    mean_commute_time + total_population + pop_density_sq_mile +
    factor(state) + factor(YEAR),
    
  data = dat_turnout
)


coeftest(model_level_turnout, vcov = vcovCL, cluster = ~state)["gas_price", ]

texreg(model_level_turnout,
       override.se = sqrt(diag(vcovCL(model_level_turnout, 
                                      cluster = ~STCOFIPS10))),
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("gas_price" = "Gas Price"),
       caption.above=TRUE)



# -------- Level Placebo: Future --------

dat_placebo_lead_turnout = dat_seds_turnout %>% 
  arrange(STCOFIPS10, YEAR) %>% 
  group_by(STCOFIPS10) %>% 
  mutate(gas_lead = lead(gas_price, 1)) %>% 
  filter(!is.na(gas_lead))



model_placebo_lead_turnout = lm(VOTER_TURNOUT_PCT ~ gas_lead + percent_car + 
                         percent_unemployment + median_income_adj2024 + 
                         median_age + percent_upper_edu + 
                         percent_white + mean_commute_time + 
                         total_population +
                         factor(state) + factor(YEAR),
                         data = dat_placebo_lead_turnout)

coeftest(model_placebo_lead_turnout, vcov = vcovCL, cluster = ~STCOFIPS10)[
  c("gas_lead"), 
]

# -------- Level Placebo: Past --------

dat_placebo_lag_turnout = dat_seds_turnout %>% 
  arrange(STCOFIPS10, YEAR) %>% 
  group_by(STCOFIPS10) %>% 
  mutate(gas_lag = lag(gas_price, 1)) %>% 
  filter(!is.na(gas_lag))



model_placebo_lag_turnout = lm(VOTER_TURNOUT_PCT ~ gas_lag + percent_car + 
                         percent_unemployment + median_income_adj2024 + 
                         median_age + percent_upper_edu + 
                         percent_white + mean_commute_time + 
                         total_population +
                         factor(state) + factor(YEAR),
                         data = dat_placebo_lag_turnout)

coeftest(model_placebo_lag_turnout, vcov = vcovCL, cluster = ~STCOFIPS10)[
  c("gas_lag"), 
]






# :::::::::::::::::::::::::::::::::::::::: HORSE RACE MODEL ::::::::::::::::::::::::::::::::::::::::
# Outdated?
# In our lagged gas price placego test, gas_lag was significant. This indicates that people
#   maybe responding to prices over time.
# So, we run a horse race model where we control for the gas_lag variable. 
# gas_price remains significant AND actually got more positive so gas_lag placebo was picking up serial correlation.
# The lagged placebo was picking up serial correlation - Once you control for current prices, past prices don't matter


dat_both = fd_dat_seds_turnout %>%
  arrange(STCOFIPS10, YEAR) %>%
  group_by(STCOFIPS10) %>%
  mutate(d_gas_lead = lead(d_gas)) %>%
  filter(!is.na(d_gas_lead))

model_both = lm(
  d_turnout ~ d_gas + d_gas_lead + percent_car + 
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_manufacturing + d_edu_health +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density +
    factor(state)+factor(YEAR),
  data = dat_both
)

coeftest(model_both, vcov = vcovCL, cluster = ~STCOFIPS10)[
  c("d_gas", "d_gas_lead"),
]


texreg(model_fd_placebo_lag_seds,
       override.se = sqrt(diag(vcovCL(model_fd_placebo_lag_seds, 
                                      cluster = ~STCOFIPS10))),
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("gas_price" = "Gas Price", "gas_lag" = "Lagged Gas Price"),
       caption.above=TRUE)


nrow(fd_dat_seds_turnout)  # Original
nrow(dat_both)  # Horse race sample








# Create dataset with both current and lagged changes (restricted sample)
dat_restricted = fd_dat_seds_turnout %>%
  arrange(STCOFIPS10, YEAR) %>%
  group_by(STCOFIPS10) %>%
  mutate(d_gas_lag = lag(d_gas)) %>%
  filter(!is.na(d_gas_lag)) %>%
  ungroup()

# Check sample sizes
cat("Full sample:", nrow(fd_dat_seds_turnout), "\n")
cat("Restricted sample:", nrow(dat_restricted), "\n")

# Main model on FULL sample (original)
model_main_full = lm(
  d_turnout ~ d_gas + percent_car + 
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_manufacturing + d_edu_health +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density +
    factor(state) + factor(YEAR),
  data = fd_dat_seds_turnout
)

# Main model on RESTRICTED sample (same as placebo sample)
model_main_restricted = lm(
  d_turnout ~ d_gas + percent_car + 
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_manufacturing + d_edu_health +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density +
    factor(state) + factor(YEAR),
  data = dat_restricted
)

# Lagged placebo on RESTRICTED sample
model_placebo_lag = lm(
  d_turnout ~ d_gas_lag + percent_car + 
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_manufacturing + d_edu_health +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density +
    factor(state) + factor(YEAR),
  data = dat_restricted
)

# Compare results
cat("\n=== MAIN MODEL - FULL SAMPLE ===\n")
print(coeftest(model_main_full, vcov = vcovCL, cluster = ~state)["d_gas", ])

cat("\n=== MAIN MODEL - RESTRICTED SAMPLE ===\n")
print(coeftest(model_main_restricted, vcov = vcovCL, cluster = ~state)["d_gas", ])

cat("\n=== LAGGED PLACEBO - RESTRICTED SAMPLE ===\n")
print(coeftest(model_placebo_lag, vcov = vcovCL, cluster = ~state)["d_gas_lag", ])
