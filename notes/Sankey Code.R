#Kelli Richardson 
#July 5, 2022
#Sankey Code 


# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggsankey)

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

# Plot ---------------------------------------------------------

Sankey <- 
  articles %>%
  #ggsankey function to format data for ggplot:
  make_long(Domain, Biomeasures, Collection, 'Frequency of feedback', Communication, Behaviors, Outcome)
# head(Sankey)

#Comprehensive Sankey 
test_sankey_full <- 
  ggplot(Sankey, aes(x = x, 
                     next_x = next_x, 
                     node = node, 
                     next_node = next_node,
                     fill = factor(node),
                     label = node)) +
  geom_sankey(flow.alpha = 0.5) +
  geom_sankey_label(size = 3.5, fill = "white") +
  theme_sankey(base_size = 16) + 
  theme(legend.position = "none") + 
  xlab(NULL)

test_sankey_full

#Notes about the above Sankey 
#The "make_long" function is the main function. This is where you list all the "columns" that are in the Sankey. The order matters here. 
#Regardless of what you change in lines 11-19, there is no need to change lines 21-32. Just run them again after making changes. 
#The "separate_rows" function is used because in some rows in the data, for instance, there are multiple biomeasures


#How to filter 
#Note - the only change from above is the addition of `filter()`

Sankey_sub <- 
  articles %>%
  filter(Biomeasures %in% c("Glucose", "Carcinomas")) %>% 
  make_long(Domain, Biomeasures, Collection, 'Frequency of feedback', Communication, Behaviors, Outcome)

test_sankey_sub <-
  ggplot(Sankey_sub, aes(x = x, 
                     next_x = next_x, 
                     node = node, 
                     next_node = next_node,
                     fill = factor(node),
                     label = node)) +
  geom_sankey(flow.alpha = 0.5) +
  geom_sankey_label(size = 3.5, fill = "white") +
  theme_sankey(base_size = 16) + 
  theme(legend.position = "none") + 
  xlab(NULL)

test_sankey_sub
