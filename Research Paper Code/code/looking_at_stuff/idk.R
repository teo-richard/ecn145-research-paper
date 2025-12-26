
dat_fd_pres_inc = read_csv("cleaned_data/merged_datasets/fd_pres_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car) %>% 
  mutate(county_fips = as.character(county_fips))

fd_guber_data = read_csv("cleaned_data/merged_datasets/fd_g_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)


# Combine the first-differenced datasets
dat_fd_all_partisan <- bind_rows(
  dat_fd_pres_inc %>% 
    select(-same_party_incumbent) %>%
    mutate(office = "President"),
  fd_guber_data %>% 
    select(-same_party_incumbent) %>%
    mutate(office = "Governor")
)

# Check the distribution
table(dat_fd_all$year, dat_fd_all$office)
table(dat_fd_all$incumbent_party, dat_fd_all$office)

# Run the combined model with party interaction
model_combined = feols(
  d_inc_vote ~ d_gas * i(incumbent_party) + 
    d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density +
    i(office) |  # Control for pres vs gov differences
    state + year,
    cluster = ~state,
    data = dat_fd_all)

summary(model_combined)

# Look specifically at the gas price coefficients
coef(model_combined)[grep("d_gas", names(coef(model_combined)))]