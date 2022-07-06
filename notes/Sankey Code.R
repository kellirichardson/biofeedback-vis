#Kelli Richardson 
#July 5, 2022
#Sankey Code 

library(tidyverse)
library(ggsankey)

# Read in data ------------------------------------------------------------

articles_raw <- read_csv("data/Data from 663 articles.csv")


# Clean data --------------------------------------------------------------

articles <- 
  articles_raw %>%
  rename(Biomeasures = `Biological measures`) %>% 
  separate_rows(Communication, sep = ",", convert = TRUE) %>% 
  separate_rows(Biomeasures, sep = ",", convert = TRUE) %>% 
  separate_rows(Behaviors, sep = ",", convert = TRUE) %>%
  separate_rows(Collection, sep = ",", convert = TRUE) %>%
  separate_rows('Frequency of feedback', sep = ",", convert = TRUE) 

#TODO: needs additional cleanup probably.  E.g. I think everything after "Other :" is supposed to be one entry?
biomeas <- unique(articles$Biomeasures)
biomeas[12:14]
#...comes from...
unique(articles_raw$`Biological measures`)[10]

#might need to split on "other" first, then split on commas


# Plot ---------------------------------------------------------

Sankey <- 
  articles %>%
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
#Note - the only change from above is on line 51 

Sankey_sub <- 
  articles %>%
  filter(Biomeasures %in% c("Glucose", "Carcinomas")) |> 
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
