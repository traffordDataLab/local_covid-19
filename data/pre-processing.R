library(tidyverse) ; library(rvest) ; library(httr) ; library(readxl) ; library(janitor) ; library(lubridate) ; library(sf)

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

# Mid-2020 population estimates
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
# Registered deaths
# -------------------------------------------

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard


# 2023 - Need to get the name of the data file dynamically as it changes each time it is updated (weekly) unlike the previous complete years
tmp <- tempfile(fileext = ".xlsx")
ext <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard") %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.xlsx") %>% 
  .[[1]]
GET(url = paste0("https://www.ons.gov.uk", ext),
    write_disk(tmp))

deaths_2023 <- read_xlsx(tmp, sheet = 4, skip = 5) %>%
  clean_names() %>%
  pivot_wider(names_from = cause_of_death, values_from = deaths) %>%
  rename(`COVID-19` = `COVID 19`) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         date = ymd("2023-01-01") + weeks(week_number),
         `Other causes` = `All causes`-`COVID-19`) %>% 
  group_by(area_code, area_name, date, week_number, place_of_death) %>% 
  summarise(`COVID-19` = sum(`COVID-19`),
            `Other causes` = sum(`Other causes`)) %>% 
  pivot_longer(-c(area_code, area_name, week_number, date, place_of_death), names_to = "cause_of_death", values_to = "number_of_deaths")

# 2022
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2022/lahbfileweek522022reg14jan231.xlsx",
    write_disk(tmp))

deaths_2022 <- read_xlsx(tmp, sheet = 4, skip = 5) %>% # 2022-11-10: previous skip value was 3, therefore there might be changes in future versions.
  clean_names() %>%
  pivot_wider(names_from = cause_of_death, values_from = deaths) %>% # 2022-11-10: previously "deaths" column was "number of deaths"
  rename(`COVID-19` = `COVID 19`) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         date = ymd("2022-01-01") + weeks(week_number),
         `Other causes` = `All causes`-`COVID-19`) %>% 
  group_by(area_code, area_name, date, week_number, place_of_death) %>% 
  summarise(`COVID-19` = sum(`COVID-19`),
            `Other causes` = sum(`Other causes`)) %>% 
  pivot_longer(-c(area_code, area_name, week_number, date, place_of_death), names_to = "cause_of_death", values_to = "number_of_deaths")

# 2021
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2021/lahbtables20215.xlsx",
    write_disk(tmp))

deaths_2021 <- read_xlsx(tmp, sheet = 4, skip = 3) %>%
  clean_names() %>%
  pivot_wider(names_from = cause_of_death, values_from = number_of_deaths) %>% 
  rename(`COVID-19` = `COVID 19`) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
         area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name),
         date = ymd("2021-01-01") + weeks(week_number),
         `Other causes` = `All causes`-`COVID-19`) %>% 
  group_by(area_code, area_name, date, week_number, place_of_death) %>% 
  summarise(`COVID-19` = sum(`COVID-19`),
            `Other causes` = sum(`Other causes`)) %>% 
  pivot_longer(-c(area_code, area_name, week_number, date, place_of_death), names_to = "cause_of_death", values_to = "number_of_deaths") 

# 2020
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek01to532020.xlsx",
    write_disk(tmp))

deaths_2020 <- read_xlsx(tmp, sheet = 4, skip = 3) %>%
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
  pivot_longer(-c(area_code, area_name, week_number, date, place_of_death), names_to = "cause_of_death", values_to = "number_of_deaths")   
  
bind_rows(deaths_2020, deaths_2021, deaths_2022, deaths_2023) %>% 
  write_csv("deaths.csv")
