# Load packages -----------------------------------------------------------
library(plotly)


# Prep data ---------------------------------------------------------------

# Not entirely sure how to get from a dataframe to this format

nodes <- tibble(
  #nodes:
  label = c("A1", "A2", "B1", "B2", "C1", "C2"),
          #    0,    1,    2,    3,    4,   5
  color = "blue"
)

links <- tibble(
  #links
  source = c(0,1,0,2,3,3), #"integer that represents the source node"
  target = c(2,3,3,4,4,5), #"integer that represents the target node"
  value =  c(8,4,2,8,4,2),#the number of articles in each link
  color = "purple"
)


# Make plot ---------------------------------------------------------------

fig <- plot_ly(
  type = "sankey",
  orientation = "h",

  node = list(
    label = nodes$label,
    color = nodes$color,
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = links$source,
    target = links$target,
    value =  links$value, # the number of articles at this node?
    color = links$color
  )
)
fig <- fig %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)

fig
