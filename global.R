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
