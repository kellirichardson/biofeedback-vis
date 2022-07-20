#Wrangle data

library(tidyverse)
library(janitor)

# Read in data ------------------------------------------------------------

articles_raw <- read_csv("data_raw/Data from 663 articles.csv")

# Clean data --------------------------------------------------------------

articles <-
  articles_raw %>%
  clean_names() %>% #make column names programming-friendly
  rename(biomeasures = biological_measures,
         feedback_freq = frequency_of_feedback) %>% 
  #separate by commas, but only ones that don't have a space after them to keep the "Other : " sentences together
  separate_rows(communication, sep = ",(?!\\s)") %>% 
  separate_rows(biomeasures, sep = ",(?!\\s)") %>%
  separate_rows(behaviors, sep = ",(?!\\s)") %>% 
  separate_rows(collection, sep = ",(?!\\s)") %>%
  separate_rows(feedback_freq, sep = ",(?!\\s)") %>%
  #replace anything that starts with "Other : " with "Other" to lump categories
  mutate(across(c(biomeasures, behaviors), ~if_else(str_detect(., "^Other : "), "Other", .)))

write_csv(articles, "app/articles_clean.csv")