# Load required libraries

library(tidyverse)
library(here)

# read in csv files

cakes <- read_csv(here("raw_data/cake-ingredients-1961.csv"))
cakes_codes <- read_csv(here("raw_data/cake_ingredient_code.csv"))

# change raw table into tidy format

cakes <- cakes %>% 
  pivot_longer(cols = c("AE":"ZH"),
               names_to = "code",
               values_to = "quantity") %>% 
  rename(cake = Cake)

# replace NAs with 0 

cakes <- cakes %>% 
  mutate(quantity = coalesce(quantity, 0))


# get names of ingredients and measures

cakes <- cakes %>% 
  left_join(cakes_codes,
            by = "code")

# reorder columns and write to .csv

cakes <- cakes %>% 
  select(cake, ingredient, quantity, measure)

write_csv(cakes, here("clean_data", "cakes_clean.csv"))
