library(tidyverse)
library(lubridate)


# --------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------
# :::::::::: HARVARD DATAVERSE DATA (DOES **NOT** INCLUDE VOTER TURNOUT, INCLUDES INCUMBENT DATA) ::::::::::
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

voter_data = read_csv("raw_data/countypres_2000-2024.csv")
dat = voter_data %>% 
    group_by(county_fips, year, party) %>% 
    summarize(
        candidatevotes = sum(candidatevotes, na.rm = TRUE),
        totalvotes = sum(totalvotes, na.rm = TRUE),
        .groups = "drop") %>% 
    mutate(vote_pct = candidatevotes / totalvotes) %>% 
    select(year, county_fips, party, vote_pct, party) %>% 
    filter(party %in% c("DEMOCRAT", "REPUBLICAN"))

dat = dat %>% group_by(county_fips) %>% pivot_wider(names_from = party, values_from = vote_pct)
names(dat)[c(3, 4)] = c("dem_vote_pct", "rep_vote_pct")


dat <- dat %>%
  mutate(
    incumbent_party = case_when(
      year == 2000 ~ "DEMOCRAT",    # Clinton
      year == 2004 ~ "REPUBLICAN",  # Bush
      year == 2008 ~ "REPUBLICAN",  # Bush
      year == 2012 ~ "DEMOCRAT",    # Obama
      year == 2016 ~ "DEMOCRAT",    # Obama
      year == 2020 ~ "REPUBLICAN",  # Trump
      year == 2024 ~ "DEMOCRAT",    # Biden
      TRUE ~ NA_character_
    ),
    incumbent_vote_pct = case_when(
      incumbent_party == "DEMOCRAT" ~ dem_vote_pct,
      incumbent_party == "REPUBLICAN" ~ rep_vote_pct,
      TRUE ~ NA_real_
    )
  )




write_csv(dat, "cleaned_data/voter_datasets/voter_data_pres_inc.csv")
