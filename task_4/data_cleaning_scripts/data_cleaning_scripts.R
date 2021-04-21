# Script to clean raw candy data


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(janitor)


# Read in raw .xls files --------------------------------------------------

candy_raw_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))
candy_raw_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx"))
candy_raw_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx"))

# Clean 2017 data ---------------------------------------------------------

# internal ID only appears in 2017 dataset - drop this.
# make a new ID for each entry which will be consistent with other datasets
# add an identifier to the ID to show the record is from 2017 dataset
# columns at end are not relevant to the task so drop

candy_2017 <- candy_raw_2017 %>% 
  select(-c("Internal ID", "Q10: DRESS":"Click Coordinates (x, y)")) %>% 
  clean_names() %>% 
  rowid_to_column("id") %>% 
  mutate(id = str_c("2017_", id))

# start to transform data into tidy format
# currently in wide format so change it to long format
# columns starting "Q6 " can be moved into a new column "candy_choice"
# values in these columns can be moved into a new column "candy_rating"

candy_2017 <- candy_2017 %>% 
  pivot_longer(cols = "q6_100_grand_bar":"q6_york_peppermint_patties",
               names_to = "candy_choice",
               names_prefix = "q6_",
               values_to = "candy_rating"
  )

# rename columns

candy_2017 <- candy_2017 %>% 
  rename(
    trick_or_treat = q1_going_out,
    gender = q2_gender,
    age = q3_age,
    country = q4_country,
    state_province = q5_state_province_county_etc,
    joy_other = q7_joy_other,
    despair_other = q8_despair_other,
    other_comments = q9_other_comments
  )

# add "year" variable to enable grouping by year once datasets are joined
# reorder columns to allow join via bind_rows

candy_2017 <-  candy_2017 %>% 
  mutate(year = 2017) %>% 
  select(sort(names(.)))

# Clean 2016 data ---------------------------------------------------------

# same process for 2016 data
# Timestamp is not a unique identifier so drop
# make a new ID for each entry which will be consistent with other datasets
# add an identifier to the ID to show the record is from 2017 dataset
# columns at end are not relevant to the task so drop

candy_2016 <- candy_raw_2016 %>% 
  select(-c("Timestamp", "Guess the number of mints in my hand.":"[York Peppermint Patties] Ignore")) %>% 
  clean_names() %>% 
  rowid_to_column("id") %>% 
  mutate(id = str_c("2016_", id))

# start to transform data into tidy format
# currently in wide format so change it to long format
# columns starting "Q6 " can be moved into a new column "candy_choice"
# values in these columns can be moved into a new column "candy_rating"

candy_2016 <- candy_2016 %>% 
  pivot_longer(cols = "x100_grand_bar":"york_peppermint_patties",
               names_to = "candy_choice",
               values_to = "candy_rating"
  )

# rename columns

candy_2016 <- candy_2016 %>% 
  rename(
    trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in,
    state_province = which_state_province_county_do_you_live_in,
    joy_other = please_list_any_items_not_included_above_that_give_you_joy,
    despair_other = please_list_any_items_not_included_above_that_give_you_despair,
    other_comments = please_leave_any_witty_snarky_or_thoughtful_remarks_or_comments_regarding_your_choices
  )

# add "year" variable to enable grouping by year once datasets are joined
# reorder columns to allow join via bind_rows

candy_2016 <-  candy_2016 %>% 
  mutate(year = 2016) %>% 
  select(sort(names(.)))


# Clean 2015 data ---------------------------------------------------------

# Same process for 2015 data
# Timestamp is not a unique identifier so drop
# make a new ID for each entry which will be consistent with other datasets
# add an identifier to the ID to show the record is from 2017 dataset
# columns at end are not relevant to the task so drop

candy_2015 <- candy_raw_2015 %>% 
  select(-c("Timestamp", "Guess the number of mints in my hand.":"Please estimate the degrees of separation you have from the following folks [Beyoncé Knowles]")) %>% 
  clean_names() %>% 
  rowid_to_column("id") %>% 
  mutate(id = str_c("2015_", id))

# start to transform data into tidy format
# currently in wide format so change it to long format
# columns starting "Q6 " can be moved into a new column "candy_choice"
# values in these columns can be moved into a new column "candy_rating"


candy_2015 <- candy_2015 %>% 
  pivot_longer(cols = "butterfinger":"york_peppermint_patties",
               names_to = "candy_choice",
               values_to = "candy_rating"
  )

# rename columns

candy_2015 <- candy_2015 %>% 
  rename(
    trick_or_treat = are_you_going_actually_going_trick_or_treating_yourself,
    age = how_old_are_you,
    joy_other = please_list_any_items_not_included_above_that_give_you_joy,
    despair_other = please_list_any_items_not_included_above_that_give_you_despair,
    other_comments = please_leave_any_remarks_or_comments_regarding_your_choices
  )

# add "year" variable to enable grouping by year once datasets are joined
# also add columns which are in 2016/2017 data but not here to ensure join is ok
# reorder columns to allow join via bind_rows

candy_2015 <-  candy_2015 %>% 
  mutate(year = 2015,
         gender = NA,
         country = NA,
         state_province = NA) %>% 
  select(sort(names(.)))


# Bind all three datasets -------------------------------------------------

# now join all three datasets together by binding columns

candy_all <- bind_rows(candy_2015, candy_2016, candy_2017)

# reorder columns for ease of visual inspection

candy_all <- candy_all %>% 
  relocate(id, age, gender, trick_or_treat, candy_choice, candy_rating,
           joy_other, despair_other, other_comments, country, state_province, year)

# can now start trying to clean the data!


# Clean "candy_choice" entries ----------------------------------------------

# Recode candy_choice entries
# No easy way of doing this using regex so has to be coded manually :-(
# Not changing joke entries/long names, only potential duplicates/misspellings


candy_all <- candy_all %>% 
  mutate(candy_choice = case_when(
    candy_choice %in% c("anonymous_brown_globs_that_come_in_black_and_orange_wrappers",
                        "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes")
    ~ "mary_janes",
    candy_choice %in% c("bonkers", "bonkers_the_candy")    ~ "bonkers",
    candy_choice == "boxo_raisins"                         ~ "box_o_raisins",
    candy_choice == "dark_chocolate_hershey"               ~ "hersheys_dark_chocolate",
    candy_choice %in% c("licorice_not_black", "licorice_yes_black")
    ~ "licorice",
    candy_choice == "sweetums_a_friend_to_diabetes"        ~ "sweetums",  #When you think of Pawnee, you think of Sweetums
    candy_choice == "x100_grand_bar"                       ~ "100_grand_bar",
    TRUE                                                   ~ candy_choice
  )
  )

# Remove joke entries from the candy_choice observations.
# Responses to "any_full_sized_candy" are not informative for assessing individual candy choices
# Therefore I have decided to remove this option from the analysis.

candy_all <- candy_all %>% 
  filter(!candy_choice %in% c("any_full_sized_candy_bar",
                              "vials_of_pure_high_fructose_corn_syrup_for_main_lining_into_your_vein",
                              "candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants",
                              "cash_or_other_forms_of_legal_tender",
                              "dental_paraphenalia",
                              "generic_brand_acetaminophen",
                              "glow_sticks",
                              "broken_glow_stick",
                              "creepy_religious_comics_chick_tracts",
                              "hugs_actual_physical_hugs",
                              "lapel_pins",
                              "pencils",
                              "peterson_brand_sidewalk_chalk",
                              "vicodin",
                              "white_bread",
                              "whole_wheat_anything",
                              "bonkers_the_board_game",
                              "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes",
                              "real_housewives_of_orange_county_season_9_blue_ray"
  ))



# Clean "age" entries -------------------------------------------------------

candy_all <- candy_all %>% 
  #remove any ".0" or similar combos to exclude decimals
  mutate(age = str_remove_all(age, "\\.[0-9]*")) %>% 
  # remove character values
  mutate(age = str_remove_all(age, "[a-zA-Z]*")) %>% 
  # remove other punctuation 
  mutate(age = str_remove_all(age, "[ ?!'\\-,>^+():]*")) %>% 
  # now change variable to be a number. Some records will be coerced to NA
  # however won't make a material difference
  mutate(age = as.numeric(age)) %>% 
  # set bounds on age range: 8 - 100
  # if outside these bounds then set to the limit
  # (tried to set to NA but was getting error as case_when needs all new values
  # to be the same variable type)
  mutate(age = case_when(
    age < 8     ~ 8,
    age <= 100  ~ age,
    age > 100   ~ 100
  )
  )


# Clean "country" entries -------------------------------------------------

# the task only asks for US/Canada/UK/Other.
# vast majority of responses are some variant of US.
# clean these and Canada/UK 
# assign all other responses to "Other" - this is accurate enough for the task.

# Send all country results to lower case and remove punctuation 
candy_all <- candy_all %>% 
  mutate(country = str_to_lower(country),
         country = str_remove_all(country, "[:punct:]")) %>% 
  arrange(country)

# Visual inspection of the data shows that all entries beginning with "u' are for USA
# with the exception of "uae", "uk", "united kingdom" and "united kindom"
# First change these to country = "UK". 
candy_all <- candy_all %>% 
  mutate(country = case_when(
    country %in% c("uk", "united kindom", "united kingdom") ~ "UK",
    TRUE ~ country)
  )

# Now convert all entries beginning "u" to "USA"
candy_all <- candy_all %>% 
  mutate(country = case_when(
    str_sub(country, 1, 1) == "u" ~ "USA",
    TRUE ~ country
  )
  )


# Cahange remaining variations of USA:
usa_variations <- c("ahemamerca",
                    "alaska",
                    "america",
                    "california",
                    "i pretend to be from canada but i am really from the united states",
                    "merica",
                    "murica",
                    "murrika",
                    "n america",
                    "new jersey",
                    "new york",
                    "north carolina",
                    "pittsburgh",
                    "subcanadian north america merica",
                    "the best one usa",
                    "the united states",
                    "the united states of america",
                    "the yoo ess of aaayyyyyy"
)

candy_all <- candy_all %>% 
  mutate(country = case_when(
    country %in% usa_variations ~ "USA",
    TRUE ~ country
  )
  )

# Change remaining variants of UK:

uk_variations <- c("endland", "england", "scotland")

candy_all <- candy_all %>% 
  mutate(country = case_when(
    country %in% uk_variations ~ "UK",
    TRUE ~ country
  )
  )

# Change variants of Canada:
canada_variations <- c("can", "canada", "canada`", "canae", "soviet canuckistan")

candy_all <- candy_all %>% 
  mutate(country = case_when(
    country %in% canada_variations ~ "Canada",
    TRUE ~ country
  )
  )

# Change all other countries to "Other"

candy_all <- candy_all %>% 
  mutate(country = case_when(
    country %in% c("Canada", "UK", "USA") ~ country,
    TRUE ~ "Other"
  )
  )


# Write clean data to .csv ------------------------------------------------

write_csv(candy_all, here("clean_data", "candy_all.csv"))



# Job done!!!! (?)
