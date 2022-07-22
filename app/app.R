library(shiny)
library(shinyWidgets) #for selectizeGroup widget
library(shinycssloaders) #for loading indicator
library(tidyverse)
library(ggsankey)

# Options to make the plot look less aliased on rsconnect
library(Cairo)
options(shiny.usecairo = TRUE)

# RStudio Connect runs relative to app/
articles  <- read_csv("articles_clean.csv")

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
    plotOutput("sankey") %>% withSpinner(type = 8),
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
  output$sankey <- renderPlot({
    #Take a dependency on the refresh button
    input$refresh
    
    #use isolate() so the plot only updates when the button is clicked, not when
    #sankey_data is updated
    #could still update highlighting with every change by using sankey_data() in
    #a scale_color* call possibly.  Worry about this later in case we don't end
    #up sticking with ggplot
    plotdf <- isolate(sankey_data()) %>% 
      ggsankey::make_long(
        domain,
        biomeasures,
        collection,
        feedback_freq,
        communication,
        behaviors,
        outcome
      )
    
    ggplot(plotdf,
           aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
      geom_sankey(flow.alpha = 0.5) +
      geom_sankey_label(size = 3.5, fill = "white") +
      scale_x_discrete(
        labels = c("Domain", "Biomeasures", "Collection",
                   "Frequency of Feedback", "Communication",
                   "Behaviors", "Outcome")
      ) +
      theme_sankey(base_size = 16) + 
      theme(legend.position = "none") + 
      xlab(NULL)
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