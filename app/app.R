library(shiny)
library(shinyWidgets) #for selectizeGroup widget
library(shinycssloaders) #for loading indicator
library(tidyverse)
library(ggsankey)
library(networkD3)
library(htmlwidgets)

# RStudio Connect runs relative to app/
articles  <- read_csv("articles_clean.csv") %>%
  mutate(biomeasures = if_else(biomeasures == "Other", "Other biomeasures", biomeasures),
         collection = if_else(collection == "Other", "Other collection types", collection),
         behaviors = if_else(behaviors == "Other", "Other behaviors", behaviors),
         outcome = if_else(outcome == "Other", "Other outcomes", outcome))

# But for development:
# articles  <- read_csv("app/articles_clean.csv")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    panel(
      selectizeGroupUI(
        id = "my-filters",
        params = list(
          domain = list(inputId = "domain", label = "Domain:"),
          biomeasures = list(inputId = "biomeasures", label = "Biomeasures:"),
          collection = list(inputId = "collection", label = "Collection:"),
          feedback_freq = list(inputId = "feedback_freq", label = "Frequency of Feedback:"),
          communication = list(inputId = "communication", label = "Communication:"),
          behaviors = list(inputId = "behaviors", label = "Behaviors:"),
          outcome = list(inputId = "outcome", label = "Outcome:")
        )
      ),
      actionButton("refresh", "Refresh Plot"),
    ),
  ),
  
  fluidRow(
    sankeyNetworkOutput(
      "sankey",
      width = "100%", #span entire page
      height = "600px" #adjust height here
    ) %>%
      withSpinner(type = 8), #loading indicator for plot
    downloadButton("download", "Download Filtered Data")
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  sankey_data <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = articles,
    vars = c("domain", "biomeasures", "collection", "outcome", 
             "feedback_freq", "communication", "behaviors", "outcome")
  )
  

  #render the plot
  output$sankey <- renderSankeyNetwork({
    #Take a dependency on the refresh button
    input$refresh
    
    #use isolate() so the plot only updates when the button is clicked, not when
    #sankey_data is updated
    #could still update highlighting with every change by using sankey_data() in
    #a scale_color* call possibly.  Worry about this later in case we don't end
    #up sticking with ggplot
    longdf <- isolate(sankey_data()) %>% 
      ggsankey::make_long(
        domain,
        biomeasures,
        collection,
        feedback_freq,
        communication,
        behaviors,
        outcome,
        value = refid
      )
    
    # Count observations for each set of links
    results_n <- longdf %>%
      group_by(node, next_node) %>%
      summarize(n = n(), 
                n_refs = length(unique(value)))
    
    # Create zero-indexed dataframe of nodes
    types <- unique(as.character(results_n$node))
    
    # Function to count # of refs per node
    count_ref <- function(x) {
      temp <- sankey_data() %>%
        filter_all(any_vars(grepl(x, .)))
      return(length(unique(temp$refid)))
    }
    N_REFS <- sapply(types, count_ref)
    
    nodes <- data.frame(node = seq(from = 0, length.out = length(types)),
                        name = types,
                        N_REFS = N_REFS)

    # Join together for links table, omit NA
    links <- left_join(results_n, nodes, by = c("node" = "name")) %>%
      left_join(nodes, by = c("next_node" = "name")) %>%
      ungroup() %>%
      rename(source = node.y,
             target = node.y.y,
             value = n) %>%
      select(source, target, value, n_refs) %>%
      na.omit() %>%
      as.data.frame()
    
    # Sankey network
    sn <- sankeyNetwork(Links = links, Nodes = nodes, Source = 'source', 
                  Target = 'target', Value = 'value', NodeID = 'name',
                  units = 'observations')
    
    # Add n_refs back into the nodes data that sankeyNetwork strips out
    sn$x$nodes$N_REFS <- nodes$N_REFS
    sn$x$links$n_refs <- links$n_refs
    
    # Modify font size
    sn$x$options$fontSize <- 10
    sn$x$options$fontFamily <- "Arial"
    
    # Add onRender JavaScript to set title to value of state
    sn <- htmlwidgets::onRender(
      sn,
      '
            function(el, x) {
                d3.selectAll(".node").select("title foreignObject body pre")
                .text(function(d) { return d.N_REFS; });
            }
            '
    )
    
    # return the result
    sn
  })
  
  # download button function
  output$download <- downloadHandler(
    filename = function() {
      #constructs file name based on today's date
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(sankey_data(), file)
    }
  )
}

shinyApp(ui, server)