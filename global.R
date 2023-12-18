library(tidyverse)

# -------------------------------------------
# Lower Tier Local Authorities
# -------------------------------------------

# Source: ONS Open Geography Portal and Nomis
# URL: https://geoportal.statistics.gov.uk
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

ltla <- read_csv("data/ltla.csv")

# -------------------------------------------
# Confirmed cases
# -------------------------------------------

# Source: Public Health England
# URL: https://coronavirus.data.gov.uk
# NOTE REGARDING CORONAVIRUS DASHBOARD DECOMMISSIONING:
#   - The original PHE coronavirus dashboard at https://coronavirus.data.gov.uk is being replaced by https://ukhsa-dashboard.data.gov.uk/topics/covid-19
#   - Final data update on the original coronavirus dashboard took place on 2023-12-14
#   - An archive of the data up to 2023-12-14 produced by the API call below was created on 2023-12-18 called "data/phe.csv"
#   - In the event of the API call below no longer working, replace with: phe <- read_csv("data/phe.csv") until we can obtain the data from the new dashboard

phe <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesByPublishDate&metric=newCasesBySpecimenDate&format=csv") %>% 
  mutate(`date` = as.Date(`date`, format = "%Y-%m-%d")) 

cases <- phe %>% 
  filter(`areaType` == "ltla") %>%
  select(date,
         area_code = `areaCode`,
         area_name = `areaName`,
         new_cases = `newCasesBySpecimenDate`) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  ungroup() %>% 
  fill(area_name) %>% 
  left_join(select(ltla, -area_name), by = "area_code") %>% 
  mutate(cum_rate = round(cum_cases/population*100000,1))

# -------------------------------------------
# Registered deaths
# -------------------------------------------

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

deaths <- read_csv("data/deaths.csv")
