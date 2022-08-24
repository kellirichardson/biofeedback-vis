library(shiny)
library(shinyWidgets) #for selectizeGroup widget
library(shinycssloaders) #for loading indicator
library(tidyverse)
library(ggsankey)
library(networkD3)
library(htmlwidgets)


# RStudio Connect runs relative to app/
articles  <- read_csv("articles_clean.csv")

# But for development:
# articles  <- read_csv("app/articles_clean.csv")


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  h1("Title"),
  p("A short description could go here, but probably shouldn't be too long or you'll have to scroll down quite a bit to get to the rest of the app."),
  fluidRow(

## Input panel -------------------------------------------------------------
    panel(
      sliderInput(
        inputId = "year_range",value = c(min(articles$year), max(articles$year)),
        label = "Year Range",
        min = min(articles$year),
        max = max(articles$year),
        sep = "",
        dragRange = TRUE
      ),
      selectizeGroupUI(
        id = "my-filters",
        params = list(
          domain = list(inputId = "domain", label = "Domain:"),
          biomarker = list(inputId = "biomarker", label = "Biomarkers:"),
          collection = list(inputId = "collection", label = "Collection:"),
          frequency = list(inputId = "frequency", label = "Frequency of Feedback:"),
          communication = list(inputId = "communication", label = "Communication:"),
          behavior = list(inputId = "behavior", label = "Behaviors:"),
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

# Filter data by selectize input ------------------------------------------
  sankey_filtered <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = articles,
    vars = c("domain", "biomarker", "collection", 
             "frequency", "communication", "behavior", "outcome")
  ) 


# Render the plot --------------------------------------------------------
  output$sankey <- renderSankeyNetwork({
    #Take a dependency on the refresh button
    input$refresh

## Filter data ------------------------------------------------------------
    #use isolate() so the plot only updates when the button is clicked, not when
    #sankey_data is updated
    #could still update highlighting with every change by using sankey_data() in
    #a scale_color* call possibly.  Worry about this later in case we don't end
    #up sticking with ggplot
    sankey_data <- isolate(sankey_filtered() %>% 
      filter(year >= input$year_range[1] & year <= input$year_range[2]))

# Prep plotting data ------------------------------------------------------

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
    # Count # of refs per node
      summarize(N_REFS = length(unique(value))) %>%
      rename(name = node) %>% 
      mutate(node = 0:(n()-1))
    
    # Join together for links table, omit NA
    links <- left_join(results_n, nodes, by = c("node" = "name")) %>%
      left_join(nodes, by = c("next_node" = "name")) %>%
      ungroup() %>%
      select(source = node.y,
             target = node.y.y,
             value = n,
             n_refs) %>%
      na.omit()
    
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
  
# Download button function ----------------------
  output$download <- downloadHandler(
    filename = function() {
      #constructs file name based on today's date
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(sankey_data, file)
    }
  )
}

shinyApp(ui, server)