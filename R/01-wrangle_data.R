#Wrangle data

library(tidyverse) 
library(janitor)

# Read in data ------------------------------------------------------------
articles_raw <- read_csv("data_raw/scoping.csv")

# Clean data --------------------------------------------------------------

articles <-
  articles_raw %>%
  clean_names() %>% #make column names programming-friendly
  rename(biomarker = "biological_measures") %>% 
  
  # Next, separate by commas, but only ones that don't have a space after them to
  # keep the "Other : " sentences together.  The ",(?!\\s)" is a "regular
  # expression" or "regex" that basically says "look for a comma that doesn't
  # have a space of any kind after it".
  separate_rows(domain, sep = ",(?!\\s)") %>% 
  separate_rows(communication, sep = ",(?!\\s)") %>% 
  separate_rows(biomarker, sep = ",(?!\\s)") %>%
  separate_rows(behavior, sep = ",(?!\\s)") %>% 
  separate_rows(collection, sep = ",(?!\\s)") %>%
  separate_rows(frequency, sep = ",(?!\\s)") %>%
  separate_rows(outcome, sep = ",(?!\\s)") %>% 
  
  # Now replace anything that starts with "Other : " with "Other" to lump
  # categories (the "^" is another bit of regex that just says this has to be at
  # the beginning of a string)
  mutate(biomarker = if_else(str_detect(biomarker, "^Other : "), "Other biomarkers", biomarker),
         collection = if_else(str_detect(collection, "^Other : "), "Other collection types", collection),
         behavior = if_else(str_detect(behavior, "^Other : "), "Other behaviors", behavior),
         outcome = if_else(str_detect(outcome, "^Other : "), "Other outcomes", outcome))

write_csv(articles, "app/articles_clean.csv")
