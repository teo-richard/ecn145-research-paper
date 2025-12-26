library(tidyverse)
library(sandwich)
library(lmtest)
library(texreg)
library(fixest)

guber_data = read_csv("cleaned_data/merged_datasets/seds_gubernatorial_inc_data.csv") %>% 
  select(-percent_manufacturing, -percent_edu_health_services, -percent_car)

fd_guber_data = read_csv("cleaned_data/merged_datasets/fd_g_inc.csv") %>% 
  select(-d_manufacturing, -d_edu_health, -d_percent_car)

fd_guber_data_noswing = fd_guber_data %>% filter(!state %in% c("FL", "OH", "PA", "MI", "WI", "AZ", "NC", "GA", "NV"))

dat_fd_g_inc_future = read_csv("cleaned_data/merged_datasets/fd_g_inc_future_placebo.csv")


dat_fd_g_inc_lag = read_csv("cleaned_data/merged_datasets/fd_g_inc_lag_placebo.csv")




# :::::::::::::::::::::::::::::::::::::::: FD ANALYSIS ::::::::::::::::::::::::::::::::::::::::
## ------ SQUARE FD MODEL -----
fd_g_mod_s = feols(
  d_inc_vote ~ d_gas + s_d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
  data = fd_guber_data
)
summary(fd_g_mod_s)

qqnorm(fd_g_mod_s$residuals)
qqline(fd_g_mod_s$residuals, col = "blue")

plot(fd_g_mod_s$residuals)
## ------ FD MODEL -----
# Regular
fd_g_mod = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
  data = fd_guber_data
)

summary(fd_g_mod)

texreg(fd_g_mod,
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("d_gas" = "Gas Price"),
       caption.above=TRUE,
       float = "H")

       texreg(fd_g_mod,
       single.row = TRUE,
       include.ci = FALSE,
       caption.above=TRUE,
       float = "H")

# :::::::::::::::::::::::::::::::::::::::: ROBUSTNESS CHECKS ::::::::::::::::::::::::::::::::::::::::
etable(fd_g_mod, fd_g_mod_county, fd_g_mod_stateyear, 
       fd_g_mod_noswing, fd_g_mod_log,
       keep = "%d_gas|d_log_gas",
       dict = c("d_gas" = "Δ Gas Price", "d_log_gas" = "Δ Log Gas Price"),
       headers = c("Main", "County FE", "2-way Cluster", "No Swing", "Log Spec"),
       tex = TRUE)

# With county and year FE
fd_g_mod_county = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    county_fips + year,
    cluster = ~state,
  data = fd_guber_data
)


summary(fd_g_mod_county)

# Two way clustering with state and year
fd_g_mod_stateyear = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state + year,
  data = fd_guber_data
)

summary(fd_g_mod_stateyear)

# No swing states
fd_g_mod_noswing = feols(
  d_inc_vote ~ d_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
  data = fd_guber_data_noswing
)


summary(fd_g_mod_noswing)


# Percent change in gas
# Log-difference (% change) instead of level difference
fd_g_mod_log = feols(
  d_inc_vote ~ d_log_gas + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density |
    state + year,
    cluster = ~state,
  data = fd_guber_data
)

summary(fd_g_mod_log)

texreg(fd_g_mod_log,
       single.row = TRUE,
       include.ci = FALSE,
       custom.coef.map = list("d_log_gas" = "Logged Gas Price"),
       caption.above=TRUE,
       float = "H")


# :::::::::::::::::::::::::::::::::::::::: PLACEBO TESTS ::::::::::::::::::::::::::::::::::::::::
# ----- FD Placebo: Future -----
# -------- Future gas prices (should be null) --------

# No Interaction
model_dat_fd_g_inc_lead = feols(
  d_inc_vote ~ d_gas_future + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_g_inc_future
)

summary(model_dat_fd_g_inc_lead)


cor(dat_fd_g_inc_future$d_gas, dat_fd_g_inc_future$d_gas_future) # -0.39


# ----- FD Placebo: Past -----
# No Interaction
model_dat_fd_g_inc_lag = feols(
  d_inc_vote ~ d_gas_lag + d_percent_pt + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_g_inc_lag
)

summary(model_dat_fd_g_inc_lag)



cor(dat_fd_g_inc_lag$d_gas, dat_fd_g_inc_lag$d_gas_lag) # also -0.42

# ~~~~~~~~~~ Horse Race for FD Placebo: Past ~~~~~~~~~~

model_gub_inc_past_hr = feols(
  d_inc_vote ~ d_gas + d_gas_lag + d_partisan_lean +
    d_unemp + d_income + d_lfpr +
    d_age + d_edu + 
    d_white + d_black + d_asian + d_native +
    d_homeowner + d_home_value +
    d_family_hh + d_married + d_hh_size +
    d_commute + d_pop + d_pop_density,
    cluster = ~state,
  data = dat_fd_g_inc_lag
)

summary(model_gub_inc_past_hr)

# Interpretation: When you control for current gas prices, past gas prices no longer matter. 

wald(model_gub_inc_past_hr, c("d_gas", "d_gas_lag"))
# This is bad bad. But that's okay. 

# :::::::::::::::::::::::::::::::::::::::: ROBUSTNESS PLOTTING ::::::::::::::::::::::::::::::::::::::::

# ----- CI PLOT -----
robustness_data = data.frame(
  model = c("Main", "County FE", "2-Way Cluster", "No Swing"),
  coef = c(-0.0991, -0.1220, -0.0991, -0.1045),
  se = c(0.0451, 0.0634, 0.0389, 0.0557)
)

robustness_data = robustness_data %>%
  mutate(
    ci_lower = coef - 1.96 * se,
    ci_upper = coef + 1.96 * se
  )

ggplot(robustness_data, aes(x = model, y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Robustness Checks: Effect of Gas Prices on Incumbent Vote Share",
    x = "Model Specification",
    y = "Coefficient Estimate (95% CI)",
    caption = "Log specification omitted"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )

ggsave("robustness_coef_plot.pdf", width = 8, height = 5)


# ------ TIME SERIES PLOT -----
seds_gas = read_csv("cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")


# First get state-level data
gas_time_states = seds_gas %>%
  group_by(year) %>%
  summarize(
    national_avg = mean(gas_price, na.rm = TRUE),
    min_price = min(gas_price, na.rm = TRUE),
    max_price = max(gas_price, na.rm = TRUE)
  ) %>% filter(year >= 2008)

ggplot(gas_time_states, aes(x = year, y = national_avg)) +
  geom_ribbon(aes(ymin = min_price, ymax = max_price), 
              fill = "steelblue", alpha = 0.2) +
  geom_line(linewidth = 1.5, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "National Average Gas Prices with State-Level Range",
    x = "Year",
    y = "Gas Price (2024 Dollars)",
    caption = "Shaded area shows range from lowest to highest state price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 11),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray30")
  )


ggsave("gas_price_timeseries.pdf", width = 8, height = 5)


# ----- Binned Scatterplot -----

library(ggplot2)

library(ggplot2)



ggplot(fd_guber_data, aes(x = d_gas, y = d_inc_vote)) +
  geom_point(alpha = 0.3, color = "gray50", size = 0.1) + 
geom_abline(intercept = 0, slope = coef(fd_g_mod_s)["d_gas"], 
              color = "darkred", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate("text", x = 0.3, y = -0.15, 
           label = "β = -0.10, p = 0.03", 
           size = 4, color = "darkred", fontface = "bold") +  # add coefficient
  labs(
    title = "Gas Price Increases Associated with Lower Incumbent Vote Share",
    x = "Change in Gas Price (dollars)",
    y = "Change in Incumbent Vote Share",
    caption = "Note: Each point represents a county-election observation. Line shows OLS fit with 95% CI."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )

ggsave("scatterplot.png", width = 8, height = 8)
