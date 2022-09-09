library(tidyverse)
library(ggsankey)
library(plotly)
library(colorspace)

#color palette function
palTableau20 <- function(n) {
  pal <- c(
    '#4E79A7','#A0CBE8','#F28E2B','#FFBE7D','#59A14F','#8CD17D',
    '#B6992D','#F1CE63','#499894','#86BCB6','#E15759','#FF9D9A',
    '#79706E','#BAB0AC','#D37295','#FABFD2','#B07AA1','#D4A6C8',
    '#9D7660','#D7B5A6'
  )
  while(n > length(pal)) {
    pal <- c(pal, pal)
  }
  pal[1:n]
}


# Load data ---------------------------------------------------------------

sankey_data <- read_csv("app/articles_clean.csv")

#if you want to manually test filters:
start_yr <- 2005
end_yr <- 2009
domain_filter <- c("Overweight/Obesity", "CVD")

# Filter data to simulate someone using the app
sankey_data <-
  sankey_data %>%
  filter(year >= start_yr, yr <= end_yr) %>% 
  filter(domain %in% domain_filter)



# Wrangle data ------------------------------------------------------------

longdf <- sankey_data %>% 
  ggsankey::make_long(
    domain,
    biomarker,
    collection,
    frequency,
    communication,
    behavior,
    outcome,
    value = refid
  )

# Count observations for each set of links
results_n <- longdf %>%
  group_by(node, next_node) %>%
  summarize(n = n(), 
            n_refs = length(unique(value)))


# Create zero-indexed dataframe of nodes
nodes <-
  longdf %>%
  group_by(node) %>%
  #count number of unique references
  summarize(n_refs = length(unique(value))) %>%
  ungroup() %>% 
  mutate(node_index = 0:(n()-1)) %>% 
  mutate(color = qualitative_hcl(n()))

# To manually set order of nodes, we need to keep information about "step" (e.g. domain, outcome, etc.).  Need a column "node_x" with numbers 0:7 corresponding to step. Also need a column "node_y" 0:? that defines order *within* each step. https://stackoverflow.com/a/65572354/8117178. Something like:

# nodes <-
#   longdf %>%
#   mutate(node_x = as.numeric(x)-1) %>%
#   group_by(x) %>%
#   mutate(node_y = as.numeric(as.factor(node))-1) %>%
#   group_by(node, node_x, node_y) %>%
#   #count number of unique references
#   summarize(n_refs = length(unique(value))) %>%
#   ungroup() %>% 
#   mutate(node_index = 0:(n()-1)) %>% 
#   mutate(color = qualitative_hcl(n()))

#then in plotly supply node_x and node_y to node = list()

# Join together for links table, omit NA
links <- 
  left_join(results_n, nodes, by = c("node" = "node")) %>%
  left_join(nodes, by = c("next_node" = "node")) %>%
  select(source = node_index.x, target = node_index.y,
         value = n, n_refs = n_refs.x, color = color.x) %>%
  na.omit()


# Plotly plot -------------------------------------------------------------
plot_ly(
  type = "sankey",
  arrangement = "perpendicular", #keeps nodes in line.  Other possible options are "snap" or "fixed"
  
  #Define nodes
  node = list(
    label = nodes$node,
    customdata = nodes$n_refs,
    color = nodes$color,
    hovertemplate = "%{label}<br>%{customdata:.d} references<extra></extra>",
    
    # styling
    pad = 20, #vertical padding between nodes
    thickness = 10, #horizontal thickness of node
    line = list(
      color = "black", #outline color
      width = 0 #outline width
    )
  ),
  
  link = list(
    source = links$source,
    target = links$target,
    value = links$value,
    color = colorspace::adjust_transparency(links$color, alpha = 0.5),
    customdata = links$n_refs,
    hovertemplate = "%{source.label} → %{target.label}<br>%{customdata:.d} references<extra></extra>"
  )
  
) %>%
  layout(
    #programmatically construct the title
    title = paste0(
      "Displaying ",
      length(unique(sankey_data$refid)),
      " papers from ",
      start_yr, 
      "-", 
      end_yr
    ),
    #set font for whole plot
    font = list(
      size = 12
    ),
    #add some margin so annotations show up
    margin = list(
      l = 100,
      r = 100,
      b = 100
    )
  ) %>%
  # add step labels
  add_annotations(
    text = c("<b>Domain</b>", "<b>Biomarker</b>", "<b>Collection Method</b>",
             "<b>Frequency of Feedback</b>", "<b>Communication</b>",
             "<b>Behavior</b>", "<b>Outcome</b>"),
    x = seq(0, 1, length.out = 7),
    y = -0.1, #below the bottom.  Use >1 for above the top.
    xanchor = "center", #center labels on steps
    showarrow = FALSE,
    font = list(
      size = 14
    )
  ) %>%
  #removes the toolbar, which doesn't do much for a Sankey diagram
  config(displayModeBar = FALSE)

