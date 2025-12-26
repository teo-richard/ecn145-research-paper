library(tidyverse)
library(readr)
library(readxl)

seds_gas_raw = read_csv("raw_data/pr_all.csv")

# Getting correct type of gas price
seds_gas = seds_gas_raw %>% filter(MSN == "MGACD") %>% select(-MSN)

seds_gas = seds_gas %>% 
    pivot_longer(cols = names(seds_gas)[3:56], names_to = "year", values_to = "gas_price") %>% 
    mutate(year = as.numeric(year))


# annual average CPI values for gasoline (all types) based on the BLS data
cpi = tribble(
  ~year, ~cpi,
  2000, 129.3,
  2002, 136.8,
  2004, 163.5,
  2006, 220.8,
  2008, 309.7,
  2010, 258.2,
  2012, 317.6,
  2014, 308.7,
  2016, 220.1,
  2018, 257.5,
  2020, 222.6,
  2022, 347.7,
  2024, 295.7
)

# Convert and inflate
seds_gas = seds_gas %>%
  mutate(gas_price_per_gallon = gas_price * 0.12) %>%  # Convert to $/gallon
  left_join(cpi, by = "year") %>%
  filter(year %in% c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2022, 2024)) %>% 
  mutate(gas_price_real2024 = gas_price_per_gallon / cpi * 314.5) %>%  # Adjust for inflation
  select(State, year, gas_price_real2024)

seds_gas = seds_gas %>% 
    pivot_wider(names_from = "State", values_from = "gas_price_real2024")


prices_2024 = read_excel("cleaned_data/gasoline_datasets/2024_gas_prices.xlsx")


prices_2024 = prices_2024 %>% 
  select(-state_fips) %>%  
  pivot_wider(names_from = state_abbr, values_from = gas_price) %>% 
  mutate(year = 2024, .before = "AL")

common_states = intersect(names(seds_gas), names(prices_2024))

seds_gas = seds_gas %>% select(all_of(common_states))
prices_2024 = prices_2024 %>% select(all_of(common_states))

together = bind_rows(seds_gas, prices_2024)

final = together %>% pivot_longer(cols = 2:52, names_to = "state", values_to = "gas_price")

# Adjustment all done, everything is in 2024 dollars

write_csv(final, "cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")

f = read_csv("cleaned_data/gasoline_datasets/cleaned_seds_plus_2024_gas_data.csv")
