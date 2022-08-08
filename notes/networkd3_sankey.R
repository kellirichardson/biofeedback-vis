library(networkD3)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggsankey)

#### Example from R Graph Gallery ####
URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
Energy <- jsonlite::fromJSON(URL)


head(Energy$links)
head(Energy$nodes)


p <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 12, nodeWidth = 30)
p


#### Test with biofeedback dataset ####

# Read in
articles_raw <- read_csv("data_raw/Data from 663 articles.csv")

# Clean
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
  mutate(across(c(Biomeasures, Behaviors), ~if_else(str_detect(., "^Other : "), "Other", .)),
         Biomeasures = if_else(Biomeasures == "Other", "Other biomeasures", Biomeasures),
         Collection = if_else(Collection == "Other", "Other collection types", Collection),
         Behaviors = if_else(Behaviors == "Other", "Other behaviors", Behaviors),
         Outcome = if_else(Outcome == "Other", "Other outcomes", Outcome))

# Use ggsankey::make_long()
Sankey <- articles %>%
  make_long(Domain, Biomeasures, Collection, 'Frequency of feedback', Communication, Behaviors, Outcome,
            value = Refid) # keep Ref ID around for summarizing

# Count number of observations for each link
results_n <- Sankey %>%
  group_by(node, next_node) %>%
  summarize(n = n(), 
            n_refs = length(unique(value)))

# Create nodes dataframe, must be zero-indexed
types <- unique(as.character(results_n$node))
count_ref <- function(x) {
  temp <- articles %>%
    select(Refid, Domain, Biomeasures, Collection, 
           'Frequency of feedback', Communication, 
           Behaviors, Outcome) %>%
    filter_all(any_vars(grepl(x, .)))
  return(length(unique(temp$Refid)))
}
N_REFS <- sapply(types, count_ref)

nodes <- data.frame(node = seq(from = 0, length.out = length(types)),
                    name = types,
                    N_REFS = N_REFS)

# Create links dataframe
links <- left_join(results_n, nodes, by = c("node" = "name")) %>%
  left_join(nodes, by = c("next_node" = "name")) %>%
  ungroup() %>%
  rename(source = node.y,
         target = node.y.y,
         value = n) %>%
  select(source, target, value, n_refs) %>%
  na.omit() %>%
  as.data.frame()

# Plot 
sn <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'observations')
sn
