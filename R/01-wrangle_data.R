#Wrangle data

library(tidyverse)

# Read in data ------------------------------------------------------------

articles_raw <- read_csv("data_raw/Data from 663 articles.csv")

# Clean data --------------------------------------------------------------

articles <-
  articles_raw %>%
  rename(Biomeasures = `Biological measures`) %>% 
  #separate by commas, but only ones that don't have a space after them to keep the "Other : " sentences together
  separate_rows(Communication, sep = ",(?!\\s)") %>% 
  separate_rows(Biomeasures, sep = ",(?!\\s)") %>%
  separate_rows(Behaviors, sep = ",(?!\\s)") %>% 
  separate_rows(Collection, sep = ",(?!\\s)") %>%
  separate_rows('Frequency of feedback', sep = ",(?!\\s)") %>%
  #replace anything that starts with "Other : " with "Other" to lump categories
  mutate(across(c(Biomeasures, Behaviors), ~if_else(str_detect(., "^Other : "), "Other", .)))

write_csv(articles, "app/articles_clean.csv")