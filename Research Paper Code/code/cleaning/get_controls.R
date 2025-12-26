library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)

# Note: This will have data for all 50 states, for only the years in the vector `election_years`

# Controls: education, median income, education, age, hispanic/latino, unemployment rate, mean commute time
# second part: getting popuplation density using acs and tigris

# Data from the ACS, see the table below for where it's from 

census_api_key("693ff38dbd441165b297753e2f7cccfa5b27c7e2")
Sys.sleep(0.5)

election_years = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2022, 2024)
# some of these variables don't start until 2008

v = load_variables(2023, "acs1")
Sys.sleep(0.5)



# We get controls for the years we want (see above) for all 50 states. Then if we need to only do certain
#   states we'll deal with that later in the merging step.
controls_all_years = map_dfr(election_years, function(yr) {
    Sys.sleep(0.5)
    
    # Education
    # B15003_001 = Total population 25+
    # B15003_022 = Bachelor's degree
    # B15003_023 = Master's degree
    # B15003_024 = Professional degree
    # B15003_025 = Doctorate degree
    b_tables <- get_acs(
        geography = "county",
        year = yr,
        survey = "acs1",
        variables = c("B15003_001", "B15003_022", "B15003_023", "B15003_024", "B15003_025"),
        cache_table = TRUE
    )
    
    Sys.sleep(0.5)
    
    # Demographics - income, total population, age
    # B19013_001 = Median household income
    # B01003_001 = Total population
    # B01002_001 = Median age
    b_income <- get_acs(
        geography = "county",
        year = yr,
        survey = "acs1",
        variables = c("B19013_001", "B01003_001", "B01002_001"),
        cache_table = TRUE
    )

    Sys.sleep(0.5)
    
    # Race (any ethnicity)
    # Note: using B02 table because the B03 table had tons of NA values.
    # Note: Couldn't get a good hispanic table without tons of NAs. 
    # B02001_001 = Total
    # B02001_002 = White alone
    # B02001_003 = Black or African American alone
    # B02001_004 = American Indian and Alaska Native alone
    # B02001_005 = Asian alone
    b_race = get_acs(
        geography = "county",
        year = yr,
        survey = "acs1",
        variables = c("B02001_001", "B02001_002", "B02001_003", 
                    "B02001_004", "B02001_005"),
        cache_table = TRUE
    )

    Sys.sleep(0.5)

    # Economic indicators
    # DP03_0005P = Unemployment rate (%)
    # DP03_0025 = Mean commute time (minutes)
    # DP03_0002P = Labor force participation rate (%)
    # DP03_0033P = Percent in manufacturing (%)
    # DP03_0046P = Percent in education/health services (%)
    # DP04_0046P = Homeownership rate (%)
    # DP04_0089 = Median home value ($)
    s_economic = get_acs(
        geography = "county",
        variables = c(
            "DP03_0005P",  # unemployment rate
            "DP03_0025",   # mean commute
            "DP03_0002P",  # labor force participation
            "DP03_0033P",  # % manufacturing
            "DP03_0046P",  # % education/health services
            "DP04_0046P",  # % homeowner
            "DP04_0089"    # median home value
        ),
        year = yr,
        survey = "acs1",
        cache_table = TRUE
    )

    Sys.sleep(0.5)
    
    # Family structure
    # DP02_0002P = Percent family households (%)
    # DP02_0007P = Percent married couple households (%)
    # DP02_0015 = Average household size
    b_family = get_acs(
        geography = "county",
        year = yr,
        survey = "acs1",
        variables = c(
            "DP02_0002P",  # % family households
            "DP02_0007P",  # % married couple
            "DP02_0015"    # avg household size
        ),
        cache_table = TRUE
    )

    Sys.sleep(0.5)

    result = bind_rows(b_tables, b_income, b_race, s_economic, b_family) %>%
        mutate(year = yr)

    Sys.sleep(1)

    return(result)
}) %>% 
    select(GEOID, NAME, variable, estimate, year)


# Pivot wider
controls = controls_all_years %>% 
    pivot_wider(names_from = "variable", values_from = "estimate")


# CPI lookup for adjusting dollar amounts to 2024 dollars
cpi_lookup = c(
    "2008" = 215.3,
    "2009" = 214.5,
    "2010" = 218.1,
    "2011" = 224.9,
    "2012" = 229.6,
    "2013" = 233.0,
    "2014" = 236.7,
    "2015" = 237.0,
    "2016" = 240.0,
    "2017" = 245.1,
    "2018" = 251.1,
    "2019" = 255.7,
    "2022" = 292.7,
    "2024" = 314.5
)


# Clean and create variables with readable names
controls = controls %>% 
    mutate(
        # Race (any ethnicity)
        percent_white = B02001_002 / B02001_001,
        percent_black = B02001_003 / B02001_001,
        percent_asian = B02001_005 / B02001_001,
        percent_native = B02001_004 / B02001_001,
        
        # Economic
        labor_force_participation = DP03_0002P,
        percent_manufacturing = DP03_0033P,
        percent_edu_health_services = DP03_0046P,
        percent_unemployment = DP03_0005P,
        median_income_adj2024 = B19013_001 * (314.5 / cpi_lookup[as.character(year)]),
        
        # Housing
        percent_homeowner = DP04_0046P,
        median_home_value_adj2024 = DP04_0089 * (314.5 / cpi_lookup[as.character(year)]),
        
        # Family structure
        percent_family_households = DP02_0002P,
        percent_married = DP02_0007P,
        avg_household_size = DP02_0015,
        
        # Education
        percent_upper_edu = (B15003_022 + B15003_023 + B15003_024 + B15003_025) / B15003_001,
        
        # Demographics
        median_age = B01002_001,
        total_population = B01003_001,
        
        # Infrastructure
        mean_commute_time = DP03_0025
    ) %>%
    # DROP ALL RAW VARIABLE CODES
    select(
        GEOID, NAME, year,
        # Keep only the cleaned variables
        percent_white, percent_black, percent_asian,
        percent_native,
        labor_force_participation, percent_manufacturing, percent_edu_health_services,
        percent_unemployment, median_income_adj2024,
        percent_homeowner, median_home_value_adj2024,
        percent_family_households, percent_married, avg_household_size,
        percent_upper_edu, median_age, total_population, mean_commute_time
    )


# Get population density
pop_data_all_years = map_dfr(election_years, function(yr) {
    pop_data = get_acs(
        geography = "county",
        variables = "B01003_001",  # Total population
        year = yr,
        survey = "acs1",
        cache_table = TRUE
    ) %>% 
        select(-moe, -variable, -NAME) %>% 
        mutate(year = yr)

    Sys.sleep(0.5)
    return(pop_data)
})

# Get county land area (in square meters)
county_geo = counties(year = 2013, cb = TRUE) %>% 
    select(GEOID, ALAND) %>% 
    st_drop_geometry()

# Calculate density (people per square mile)
density = pop_data_all_years %>% 
    left_join(county_geo, by = "GEOID") %>% 
    mutate(
        pop_density_sq_mile = estimate / (ALAND / 2589988.11)  # Convert sq meters to sq miles
    ) %>% 
    select(GEOID, year, pop_density_sq_mile)


# Final merge
controls_final = inner_join(controls, density, by = c("GEOID", "year"))

controls_final_clean = controls_final %>% 
    filter(total_population >= 65000) %>% # Getting only counties above 65k per ACS1 rules
    filter(!is.na(percent_white)) %>%
    drop_na()  # Remove any remaining rows with NAs

# Check data quality
cat("Final dataset has", nrow(controls_final_clean), "rows and", ncol(controls_final_clean), "columns\n")

cat("\nCounties per year:\n")
controls_final_clean %>%
    group_by(year) %>%
    summarize(n_counties = n()) %>%
    print()



# Save
write.csv(controls_final_clean, "cleaned_data/general_datasets/cleaned_controls.csv", row.names = FALSE)

