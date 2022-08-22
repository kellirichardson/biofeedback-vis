library(shiny)
library(shinyWidgets) #for selectizeGroup widget
library(shinycssloaders) #for loading indicator
library(tidyverse)
library(ggsankey)

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
    plotOutput(
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
  output$sankey <- renderPlot({
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
    
    plotdf <- sankey_data %>% 
      ggsankey::make_long(
        domain,
        biomarker,
        collection,
        frequency,
        communication,
        behavior,
        outcome
      )

# Build the Plot --------------------------------------------------------
    ggplot(plotdf,
           aes(x = x, 
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
      geom_sankey_label(
        size = 3.5,
        fill = "white",
        color = "darkblue", #outline and text color
        family = "Arial", #set label font
        label.padding = unit(0.2, "lines"), #padding between text and label outline
        label.size = 0.2, #thickness of line around label
        label.r = unit(0.1, "lines") #roundness of label corners
      ) +
      
      #set x-axis labels manually
      scale_x_discrete(
        labels = c("Domain", "Biomarkers", "Collection",
                   "Frequency of Feedback", "Communication",
                   "Behaviors", "Outcome")
      ) +
      theme_sankey(base_size = 16) + 
      theme(legend.position = "none") + 
      xlab(NULL)
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