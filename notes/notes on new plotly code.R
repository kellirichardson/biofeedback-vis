library(tidyverse)
library(ggsankey)
library(networkD3)

df <- tribble(
  ~doi, ~domain, ~freq, ~outcome,
  1   , "diabetes", "once", "blood sugar reduction",
  1   , "diabetes", "once", "weight loss",
  2   , "diabetes", "more than once", "blood sugar reduction",
  3   , "obesity", "once", "weight loss",
  3   , "obesity", "once", "activity level", #TODO tooltip from obsity to once should be 1 paper.
  4   , "obesity", "more than once", "weight loss",
  5   , "obesity", "more than once", "weight loss"
)


#ggsankey version:

df_made_long <- ggsankey::make_long(df, domain, freq, outcome, value = doi) 

ggplot(df_made_long, 
       aes(x = x, 
           next_x = next_x, 
           node = node, 
           next_node = next_node,
           fill = factor(node),
           label = node)
) +
  geom_sankey() +
  geom_sankey_label() +
  theme(legend.position = "none")


# Networkd3 version:

# Count number of observations for each link
results_n <-
  df_made_long %>%
  group_by(node, next_node) %>%
  summarize(n = n(), 
            n_refs = length(unique(value)))

# nodes
nodes <-
  df_made_long %>%
  group_by(node) %>%
  summarize(N_REFS = length(unique(value))) %>%
  rename(name = node) %>% 
  mutate(node = 0:(n()-1)) %>% 
  as.data.frame()


# edges
# Create links dataframe
links <- 
  left_join(results_n, nodes, by = c("node" = "name")) %>%
  left_join(nodes, by = c("next_node" = "name")) %>%
  ungroup() %>%
  rename(source = node.y,
         target = node.y.y,
         value = n) %>%
  select(source, target, value, n_refs) %>%
  na.omit() %>%
  as.data.frame()

sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
              Target = 'target', Value = 'value', NodeID = 'name',
              units = 'observations')

# try a plotly version
library(plotly)
plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = nodes$name,
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  #TODO: put both numbers of observations and number of # refs/papers
  link = links,
  hoverinfo = "text",
  # hoverlabel = ,
)
