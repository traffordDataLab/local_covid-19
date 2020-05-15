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

cases <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv",
                  col_types = cols(
                    `Area name` = col_character(),
                    `Area code` = col_character(),
                    `Area type` = col_character(),
                    `Specimen date` = col_date(format = "%Y-%m-%d"),
                    `Daily lab-confirmed cases` = col_integer(),
                    `Cumulative lab-confirmed cases` = col_integer())) %>% 
  filter(`Area type` == "Lower tier local authority") %>%
  select(date = `Specimen date`,
         area_code = `Area code`,
         area_name = `Area name`,
         new_cases = `Daily lab-confirmed cases`) %>% 
  arrange(date) %>% 
  group_by(area_code, area_name) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  ungroup() %>% 
  fill(area_name)

# -------------------------------------------
# Registered deaths
# -------------------------------------------

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

deaths <- read_csv("data/deaths.csv")
