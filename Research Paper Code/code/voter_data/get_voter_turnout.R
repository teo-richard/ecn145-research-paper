library(tidyverse)
library(lubridate)

# NOTE!!! The NANDA data includes voter turnout. The Harvard Dataverse data does not include voter turnout. 


# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------


# :::::::::: NANDA DATA (**INCLUDES** VOTER TURNOUT) ::::::::::
# https://nanda.isr.umich.edu/project/voter-registration-turnout-and-partisanship/

voter_data_raw = read_tsv("raw_data/ICPSR_38506/DS0001/38506-0001-Data.tsv")

# voter_data = voter_data_raw %>% 
#     select(STCOFIPS10, YEAR, VOTER_TURNOUT_PCT, REG_VOTER_TURNOUT_PCT) %>% 
#     filter(str_starts(STCOFIPS10, "06|08|12|25|27|36|39|48|53")) # the states that are in the EIA gas dataset
# ^ you do this in the `merge_eia_for_turnout.R` file :)

voter_data = voter_data_raw %>% 
    select(STCOFIPS10, YEAR, VOTER_TURNOUT_PCT, REG_VOTER_TURNOUT_PCT, PARTISAN_INDEX_DEM)

voter_data = voter_data %>% 
    filter(YEAR >= 2010)

write_csv(voter_data, "cleaned_data/voter_datasets/general_cleaned_voter_data.csv")
