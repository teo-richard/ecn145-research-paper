library(tidyverse)

load("raw_data/county_guber_voter_data/dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata")
# gov_elections_release
# https://dataverse.harvard.edu/citation?persistentId=doi:10.7910/DVN/DGUMFI

# Note: seat_status: String variable capturing the status of the given seat at the time of the election


gov_elections = gov_elections_release %>% 
    filter(election_year >= 2000) %>% 
    select(
        county_fips = fips,
        year = election_year,
        state,
        county_name,
        dem_votes = democratic_raw_votes,
        rep_votes = republican_raw_votes,
        seat_status,
        total_votes = raw_county_vote_totals
    ) %>%
    mutate(
        two_party_total = dem_votes + rep_votes,
        dem_vote_pct = dem_votes / two_party_total,
        rep_vote_pct = rep_votes / two_party_total,

        incumbent_party = case_when(
            seat_status == "Democratic Incumbent" ~ "DEMOCRAT",
            seat_status == "Democratic Open Seat" ~ "DEMOCRAT",  # Dem held seat, not running
            seat_status == "Republican Incumbent" ~ "REPUBLICAN",
            seat_status == "Republican Open Seat" ~ "REPUBLICAN",  # Rep held seat, not running
            seat_status == "Third-Party/Independent Open Seat" ~ NA_character_,  # Skip these
            TRUE ~ NA_character_
        ),
        # ^Note: we don't care if the guy isn't running again bc we just care about the votes the party itself is getting not the particular dude
        
        incumbent_vote_pct = case_when(
            incumbent_party == "DEMOCRAT" ~ dem_vote_pct,
            incumbent_party == "REPUBLICAN" ~ rep_vote_pct,
            TRUE ~ NA_real_
        )
    ) %>%
    
    # Remove rows with missing vote data
    filter(!is.na(dem_vote_pct) & !is.na(rep_vote_pct) & !is.na(incumbent_party))



write_csv(gov_elections, "cleaned_data/voter_datasets/guber_voter_data.csv")


