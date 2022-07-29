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
  geom_sankey(
    flow.alpha = 0.5,
    node.color = "gray30", #color of node outline
    width = 0.1 #node width
    ) +
  
  #appears to accept args passed to geom_label()
  geom_sankey_label(
    size = 3.5,
    fill = "white",
    color = "darkblue", #outline and text color
    family = "Avenir", #sets label font
    label.padding = unit(0.2, "lines"), #padding between text and label outline
    label.size = 0.2, #thickness of line around label
    label.r = unit(0.1, "lines") #roundness of labels
  ) +
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
  make_long(Domain, Biomeasures, Collection, 'Frequency of feedback', Communication, Behaviors, Outcome) %>% 
  mutate(across(everything(), as.factor))

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



# Highlighting --------------------------------------------------------------

#' We want to be able to render nodes that will be removed when the plot is
#' re-drawn as greyed out. 
#' 
#' Ok, let's start with a filtered version of articles
#' as our test data

full_data <- articles %>% filter(Domain == "Diabetes")
full_data_sankey <- make_long(full_data, Domain, Biomeasures, Collection, 'Frequency of feedback', Communication, Behaviors, Outcome)
p <- 
  ggplot(full_data_sankey,
       aes(x = x, 
           next_x = next_x, 
           node = node, 
           next_node = next_node,
           fill = factor(node),
           label = node)) +
  geom_sankey() +
  geom_sankey_label() +
  theme(legend.position = "none")
p

#' Let's say we want to filter out "Device" from the Communication step.
#' 

step_rm <- "Communication"
node_rm <- "Device"

#' Before plotting the filtered plot, we want to grey out any nodes connected to
#' "Device" in .  To do this, we need to 1) figure out what nodes are to
#' the left of Self-measurement (flows to the right will get greyed out with
#' Self-measurment as they take on the same fill) and 2) replace that color with
#' grey in the scale.
#' 
#' Let's start by making a manual scale that we can edit

nodes <- unique(c(full_data_sankey$node, full_data_sankey$next_node))
colors <- colorspace::qualitative_hcl(n = length(nodes))
names(colors) <- nodes

p + scale_fill_manual(values = colors)

#' Now, figure out what node is connected to self-measurment
#' 
connections_df <- 
  full_data_sankey %>% 
  filter(
    next_x == step_rm,
    node == node_rm |
      next_node == node_rm
  )

# Now we set those colors to grey

to_grey <- unique(c(connections_df$node, connections_df$next_node))
to_grey

colors_greyed <- colors

colors_greyed[names(colors_greyed) %in% to_grey] <- "grey50"

# If we only want to grey out flows coming out of "Device" then it's easier:

colors_greyed2 <- colors
colors_greyed2[names(colors_greyed2) %in% node_rm] <- "grey50"


#now plot with greyed out

p + scale_fill_manual(values = colors_greyed)
p + scale_fill_manual(values = colors_greyed2)


# So the problem here is that "Other" shows up in multiple steps and they all
# get greyed out.  Otherwise this would work.  A possible work-around is to give the "other"s unique names.