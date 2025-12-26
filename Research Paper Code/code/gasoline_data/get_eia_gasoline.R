library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
library(stringr)

# Note: This will have data for only 9 states, for only the years in the vector `election_years`
# The EIA spreadsheet only has 9 states (California|Colorado|Florida|Massachusetts|Minnesota|New York|Ohio|Texas|Washington)

election_years = c(2008, 2010, 2012, 2014, 2016, 2018, 2022, 2024)

gas_data_raw = read_xls("raw_data/pswrgvwall.xls", sheet = 4)

descriptions = gas_data_raw[2, ]

state_names <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
                 "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
                 "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                 "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
                 "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# Find columns where description contains any state name
state_pattern = paste(state_names, collapse = "|")
state_cols = which(str_detect(descriptions, state_pattern))

matched_states = state_names[sapply(state_names, function(s) any(str_detect(descriptions, s)))]
print(matched_states)

state_price_names = unname(sapply(matched_states, function(x) paste0(x, "_price")))



# Select only those columns (plus maybe date column)
gas_data = gas_data_raw[-c(1,2), c(1, state_cols)] %>% na.omit() %>% select(-...27)
colnames(gas_data) = c("date", state_price_names)
gas_data = gas_data %>% mutate(across(everything(), as.numeric))
gas_data$date = as.Date(gas_data$date, origin = "1899-12-30")


gas_data = gas_data %>% 
    filter(month(date) %in% c(9, 10)) %>% 
    filter(year(date) %in% election_years)

gas_data = gas_data %>%
  group_by(year = year(date)) %>% 
  summarize(across(
    ends_with("_price"),
    ~mean(.x, na.rm = TRUE),
    .names = "{.col}_sep_oct_avg"
  ))


# CPI values (annual average CPI-U)
cpi = tribble(
  ~year, ~cpi,
  2008, 215.3,  # Need to add this
  2010, 218.1,
  2012, 229.6,
  2014, 236.7,
  2016, 240.0,
  2018, 251.1,
  2020, 258.8,  # Need to add this
  2022, 292.7,
  2024, 314.5
) %>%
  mutate(cpi_adjustment = 314.5 / cpi)

# Merge and adjust all gas price columns to 2024 dollars
gas_data = gas_data %>%
  left_join(cpi, by = "year") %>%
  mutate(across(
    ends_with("_sep_oct_avg"),
    ~.x / cpi * 314.5,
    .names = "{.col}_real2024"
  )) %>%
  select(-ends_with("_sep_oct_avg"), -cpi)  # Drop nominal prices and cpi column



write_csv(gas_data, "cleaned_data/gasoline_datasets/cleaned_eia_gas_data.csv")

