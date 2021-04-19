
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
  mutate(athlete = str_to_title(athlete)) %>% 
  rename(decathlon_rank = rank,
         decathlon_total = points) %>% 
  relocate(decathlon_rank, .after = last_col())

# Tidy data
# Make "event" and "event_result" variables
# Move variables that are currently columns into these new variables

decathlon_clean <- decathlon_clean %>% 
  pivot_longer(cols = c("x100m":"decathlon_total"),
               names_to = "event",
               values_to = "event_result")
 


# Write cleaned data to .csv file

write_csv(decathlon_clean, here("clean_data", "decathlon_clean.csv"))



