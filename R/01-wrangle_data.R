#Wrangle data

library(tidyverse) 
library(janitor)

# Read in data ------------------------------------------------------------

articles_raw <- read_csv("data_raw/Scoping.csv")

# Clean data --------------------------------------------------------------
View(articles_raw %>% clean_names())
articles <-
  articles_raw %>%
  clean_names() %>% #make column names programming-friendly
  
  # Next, separate by commas, but only ones that don't have a space after them to
  # keep the "Other : " sentences together.  The ",(?!\\s)" is a "regular
  # expression" or "regex" that basically says "look for a comma that doesn't
  # have a space of any kind after it".
  separate_rows(communication, sep = ",(?!\\s)") %>% 
  separate_rows(biomarker, sep = ",(?!\\s)") %>%
  separate_rows(behavior, sep = ",(?!\\s)") %>% 
  separate_rows(collection, sep = ",(?!\\s)") %>%
  separate_rows(frequency, sep = ",(?!\\s)") %>%
  
  # Now replace anything that starts with "Other : " with "Other" to lump
  # categories (the "^" is another bit of regex that just says this has to be at
  # the beginning of a string)
  mutate(
    across(c(biomarker, behavior), 
           ~if_else(str_detect(., "^Other : "), "Other", .))
  )

write_csv(articles, "app/articles_clean.csv")