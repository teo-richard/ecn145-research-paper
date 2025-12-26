t = dat_seds_incumbent %>% select(county_fips, gas_price, year)

a = t %>% pivot_wider(id_cols = county_fips, names_from = "year", values_from = "gas_price")

a1 = a$`2012`
a2 = a$`2016`
a3 = a$`2024`

cor(a %>% select(-county_fips), use = "na.or.complete")
