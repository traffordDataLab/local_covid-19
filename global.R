library(tidyverse) ; library(sf)

# -------------------------------------------
# MSOA
# -------------------------------------------

# Source: ONS Open Geography Portal and Nomis
# URL: https://geoportal.statistics.gov.uk
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

msoa <- st_read("data/msoa.geojson")

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

phe <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% 
  mutate(`Specimen date` = as.Date(`Specimen date`, format = "%Y-%m-%d")) 

cases <- phe %>% 
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

# -------------------------------------------
# Clinical vulnerabilities
# -------------------------------------------

# Source: NHS Digital ; House of Commons Library
# URL: https://commonslibrary.parliament.uk/social-policy/health/constituency-data-how-healthy-is-your-area/

clinical_vulnerabilities <- read_csv("data/clinical_vulnerabilities.csv")

