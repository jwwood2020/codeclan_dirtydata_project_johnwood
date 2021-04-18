
# Script to read in and clean 'decathlon' dataset


# Load required libraries

library(tidyverse)
library(janitor)
library(here)


# Read in raw data 

decathlon_raw <- read_rds(here("raw_data/decathlon.rds"))


# Clean data

decathlon_clean <- decathlon_raw %>%
  rownames_to_column("athlete") %>% 
  clean_names() %>% 
  mutate(athlete = str_to_title(athlete))


# Write cleaned data to .csv file

write_csv(decathlon_clean, here("clean_data", "decathlon_clean.csv"))



