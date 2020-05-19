library(tidyverse) ; library(httr) ; library(readxl) ; library(janitor) ; library(lubridate) ; library(sf)

# -------------------------------------------
# MSOA
# -------------------------------------------

# MSOA names
# Source: House of Commons Library
# URL: https://visual.parliament.uk/msoanames

msoa_names <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-v1.1.0.csv") %>% 
  select(msoa11cd, msoa11hclnm)

# MSOA > LAD
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales/datas

# NB combine Cornwall and Isles of Scilly 

msoa_codes <- read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv") %>% 
  setNames(tolower(names(.))) %>% 
  distinct(msoa11cd, msoa11nm, lad11cd, lad11nm) %>% 
  filter(str_detect(msoa11cd, "^E")) %>% 
  mutate(lad11cd = case_when(as.character(lad11cd) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ lad11cd),
         lad11nm = case_when(lad11cd == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ lad11nm)) %>% 
  left_join(msoa_names, by = "msoa11cd") %>% 
  select(msoa11cd, msoa11nm, msoa11hclnm, area_code = lad11cd, area_name = lad11nm)

# MSOA vector boundaries
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bsc

# join datasets and write as GeoJSON

st_read("https://opendata.arcgis.com/datasets/c661a8377e2647b0bae68c4911df868b_3.geojson") %>% 
  filter(str_detect(msoa11cd, "^E")) %>% 
  select(msoa11cd) %>% 
  left_join(msoa_codes, by = "msoa11cd") %>% 
  st_write("msoa.geojson")

# -------------------------------------------
# Lower Tier Local Authorities
# -------------------------------------------

# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales/data

# NB combine Cornwall and Isles of Scilly 

ltla <- read_csv("https://opendata.arcgis.com/datasets/3e4f4af826d343349c13fb7f0aa2a307_0.csv") %>% 
  select(area_code = LTLA19CD, area_name = LTLA19NM) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name)) %>% 
  distinct(area_code, .keep_all = TRUE)

# Mid-2018 population estimates
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala

# NB combine population estimates for Cornwall and Isles of Scilly

population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population))

# join datasets and write as CSV

left_join(ltla, population, by = "area_code") %>%
  write_csv("ltla.csv")

# -------------------------------------------
# Clinical vulnerabilities
# -------------------------------------------

# Source: House of Commons Library
# URL: https://commonslibrary.parliament.uk/social-policy/health/constituency-data-how-healthy-is-your-area

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://data.parliament.uk/resources/constituencystatistics/HealthDiseasePrevalence.xlsx",
    write_disk(tmp))

read_xlsx(tmp, sheet = 4) %>% 
  drop_na() %>% 
  select(msoa11cd = `Row Labels`,
         Asthma, COPD, `Chronic Kidney Disease`, `Coronary Heart Disease`,
         Diabetes, `High Blood Pressure`, Obesity) %>% 
  write_csv("clinical_vulnerabilities.csv")

# -------------------------------------------
# Registered deaths - up to 1 May 2020
# -------------------------------------------

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard

tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek19.xlsx",
    write_disk(tmp))

read_xlsx(tmp, sheet = 4, skip = 3) %>% 
  clean_names() %>% 
  pivot_wider(names_from = cause_of_death, values_from = number_of_deaths) %>% 
  rename(`COVID-19` = `COVID 19`) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         date = ymd("2019-12-27") + weeks(week_number),
         `Other causes` = `All causes`-`COVID-19`) %>% 
  group_by(area_code, area_name, date, week_number, place_of_death) %>% 
  summarise(`COVID-19` = sum(`COVID-19`),
            `Other causes` = sum(`Other causes`)) %>% 
  pivot_longer(-c(area_code, area_name, week_number, date, place_of_death), names_to = "cause_of_death", values_to = "number_of_deaths") %>% 
  write_csv("deaths.csv")



