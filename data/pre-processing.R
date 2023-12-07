library(tidyverse) ; library(rvest) ; library(httr) ; library(readxl) ; library(janitor) ; library(lubridate) ; library(sf)

# -------------------------------------------
# Lower Tier Local Authorities (April 2021)
# -------------------------------------------

# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/ons::lower-tier-local-authority-to-upper-tier-local-authority-april-2021-lookup-in-england-and-wales-1/about

# NB combine Cornwall and Isles of Scilly 

ltla <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LTLA21_UTLA21_EW_LU_9bbac05558b74a88bda913ad5bf66917/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>%
    st_drop_geometry() %>%
    select(area_code = LTLA21CD, area_name = LTLA21NM) %>% 
    filter(str_detect(area_code, "^E")) %>% 
    mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code),
           area_name = case_when(area_code == "E06000052" ~ "Cornwall and Isles of Scilly", TRUE ~ area_name)) %>% 
    distinct(area_code, .keep_all = TRUE)

# -------------------------------------------
# Mid-2022 population estimates (released 2023-11-29) for Local Authorities as of April 2021
# -------------------------------------------

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala -> https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2002
# NOMIS selections:
#   - Geography: local authorities: district / unitary (as of April 2021) [All]
#   - Date [2022]
#   - Age [All Ages]
#   - Sex [Total]

# NB combine population estimates for Cornwall and Isles of Scilly

population <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939329...1811939332,1811939334...1811939336,1811939338...1811939428,1811939436...1811939442,1811939768,1811939769,1811939443...1811939497,1811939499...1811939501,1811939503,1811939505...1811939507,1811939509...1811939517,1811939519,1811939520,1811939524...1811939570,1811939575...1811939599,1811939601...1811939628,1811939630...1811939634,1811939636...1811939647,1811939649,1811939655...1811939664,1811939667...1811939680,1811939682,1811939683,1811939685,1811939687...1811939704,1811939707,1811939708,1811939710,1811939712...1811939717,1811939719,1811939720,1811939722...1811939730,1811939757...1811939767&date=latest&gender=0&c_age=200&measures=20100") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  mutate(area_code = case_when(as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", TRUE ~ area_code)) %>% 
  # The following 2 lines are to get the combined total population for Cornwall and the Isles of Scilly as they will now have the same area_code: E06000052
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
