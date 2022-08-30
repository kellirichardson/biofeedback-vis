library(tidyverse)
library(ggsankey)
library(plotly)
library(colorspace)

df <- tribble(
  ~doi, ~domain, ~freq, ~outcome,
  1   , "diabetes", "once", "blood sugar reduction",
  1   , "diabetes", "once", "weight loss",
  2   , "diabetes", "more than once", "blood sugar reduction",
  3   , "obesity", "once", "weight loss",
  3   , "obesity", "once", "activity level", #TODO tooltip from obsity to once should be 1 paper.
  4   , "obesity", "more than once", "weight loss",
  5   , "obesity", "more than once", "weight loss",
  6   , "diabetes", "once", "weight loss"
)


df_made_long <- ggsankey::make_long(df, domain, freq, outcome, value = doi) 

# Count number of observations for each link
results_n <-
  df_made_long %>%
  group_by(node, next_node) %>%
  summarize(n = n(), 
            n_refs = length(unique(value)))

#TODO: add colors in the nodes and links dataframes (or earlier?)
# nodes
nodes <-
  df_made_long %>%
  group_by(node) %>%
  summarize(n_refs = length(unique(value))) %>%
  rename(name = node) %>% 
  mutate(node = 0:(n()-1)) %>% 
  mutate(color = qualitative_hcl(n(), alpha = 0.5))


# edges
# Create links dataframe
links <- 
  left_join(results_n, nodes, by = c("node" = "name")) %>%
  left_join(nodes, by = c("next_node" = "name")) %>%
  select(source = node.y, target = node.y.y, value = n, n_refs = n_refs.x, color = color.x) %>%
  na.omit()


# Make plot ---------------------------------------------------------------

plot_ly(
  type = "sankey",
  arrangement = "perpendicular", #keeps nodes in line.  Other possible options are "snap" or "fixed"
  
  #Define nodes
  node = list(
    label = nodes$name,
    customdata =  nodes$n_refs,
    color = nodes$color,
    hovertemplate = "%{label}<br>%{customdata:.d} references<extra></extra>",
    
    #not sure what this does.  Was hoping it would add axis labels
    groups = list(
      "domain" = c(4,2),
      "freq" = c(3,5),
      "outcome" = c(0,1,6)
    ),
    
    # styling
    pad = 20, #vertical padding between nodes
    thickness = 10, #horizontal thickness of node
    line = list(
      color = "black", #outline color
      width = 0.5 #outline width
    )
  ),
  
  link = list(
    source = links$source,
    target = links$target,
    value = links$value,
    color = links$color, #TODO: adjust alpha
    opacity = 0.5,
    customdata =  links$n_refs,
    hovertemplate = "%{customdata:.d} references<br> From '%{source.label}' to '%{target.label}<extra></extra>'"
  )
  
) %>% 
  layout(
    #TODO: fill the title in programmatically.
    # title = paste0("showing ", length(unique(df$doi)), " papers from ", minyr, "-", maxyr),
    font = list(
      size = 12
    ),
    xaxis = list(showgrid = F, zeroline = F)
  ) %>% 
  # add step labels
  add_annotations(
    text = c("domain", "freq", "outcome"),
    x = c(0, 0.5, 1),
    y = -0.1, #below the bottom.  Use 1.1 for above the top.
    showarrow = FALSE
  ) %>% 
  #removes the toolbar, which doesn't do much for a Sankey diagram
  config(displayModeBar = FALSE)


# https://python.plainenglish.io/sankeying-with-plotly-90500b87d8cf

