# Load in required libraries

library(tidyverse)
library(readxl)
library(here)

# Read in raw data spreadsheet

bird_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Bird data by record ID")
ship_data <- read_excel(here("raw_data/seabirds.xls"), sheet = "Ship data by record ID")

# Keep only required columns for bird_data and rename

bird_data <- bird_data %>% 
  select(RECORD,
         `RECORD ID`,
         `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
         `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
         `Species abbreviation`,
         COUNT) %>% 
   rename(record = RECORD,
         id = `RECORD ID`,
         common_name = `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
         scientific_name = `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
         abbreviation = `Species abbreviation`,
         sightings = COUNT)

# Keep only required columns for ship_data and rename

ship_data <- ship_data %>% 
  select(`RECORD ID`,
         LAT,
         LONG) %>% 
  rename(id = `RECORD ID`,
         latitude = LAT,
         longitude = LONG)

# Join bird_data with ship_data to get latitude/longitude

bird_data <- bird_data %>% 
  left_join(ship_data,
            by = "id")

# Remove records where there was no sighting

bird_data <- bird_data %>% 
  filter(common_name != "[NO BIRDS RECORDED]")


# Write cleaned data to .csv file

write_csv(bird_data, here("clean_data", "bird_data.csv"))

