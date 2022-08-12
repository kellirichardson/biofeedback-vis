#Wrangle data

library(tidyverse) 
library(janitor)
library(forcats)

# Read in data ------------------------------------------------------------
articles_raw <- read_csv("data_raw/Scoping.csv")

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
  mutate(
    across(c(biomarker, behavior), 
           ~if_else(str_detect(., "^Other : "), "Other", .))
  )
  
articles$domain <- rename (articles$domain, 'Neurodegenerative disease' = 'Alzheimer’s disease') # not working. 
# need to also rename obstructive sleep apnea as sleep apnea

#Forcats experiment (I have no idea whether this goes here)
articles$domain_lumped <- fct_lump_min(articles$domain, 5 , other_level = "Other") #not sure if 5 is appropriate, just experimenting
table (articles$domain_lumped) #now I'm confused since it's listing more articles per Diabetes domain than in our entire database!
#ideally we would want to also do this for outcome, and behavior 

write_csv(articles, "app/articles_clean.csv")
