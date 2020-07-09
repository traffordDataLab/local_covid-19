# Weekly rate of COVID-19 cases by UTLA by Pillar 1 and 2 testing
# Source: National COVID-19 surveillance reports, Public Health England
# URL: https://www.gov.uk/government/publications/national-covid-19-surveillance-reports

library(tidyverse) ; library(rvest) ; library(httr) ; library(readxl)

# current weekly rate
tmp <- tempfile(fileext = ".xlsx")
url <- read_html("https://www.gov.uk/government/publications/national-covid-19-surveillance-reports") %>% 
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.xlsx") %>% 
  .[[1]]

GET(url = url,
    write_disk(tmp))

current_week <- read_xlsx(tmp, sheet = 10, skip = 7) %>% 
  select(area_code = `UTLA code`, area_name = `UTLA name`, 
         current_week = `Rate per 100,000 population`) %>%
  filter(!is.na(area_name)) %>% 
  mutate(current_week = as.numeric(current_week),
         current_week = replace_na(current_week, 0))

# previous weekly rate
# change the URL!
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/897200/Weekly_COVID19_report_data_w27.xlsx",
    write_disk(tmp))

previous_week <- read_xlsx(tmp, sheet = 10, skip = 7) %>% 
  select(area_code = `UTLA code`, area_name = `UTLA name`, 
         previous_week = `Rate per 100,000 population`) %>%
  filter(!is.na(area_name)) %>% 
  mutate(previous_week = as.numeric(previous_week),
         previous_week = replace_na(previous_week, 0))

# Join weekly rates
weekly_rates <- previous_week %>% 
  left_join(select(current_week, -area_name), by = "area_code") %>% 
  mutate(previous_week = round(previous_week,1),
         current_week = round(current_week,1),
         change = round(current_week-previous_week,1))

# Write data
write_csv(weekly_rates, "weekly_rates.csv")
