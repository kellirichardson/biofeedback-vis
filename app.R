library(shiny)
library(tidyverse)

articles  <- read_csv("data/articles_clean.csv")

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    #Filter section.  For now just two sets of checkboxes
    column(2,
           checkboxGroupInput(
             inputId = "Domain",
             label = "Domain",
             choices = unique(articles$Domain),
             selected = unique(articles$Domain)
           )
    ),
    column(2,
           checkboxGroupInput(
             inputId = "Biomeasures",
             label = "Biomeasures",
             choices = unique(articles$Biomeasures),
             selected = unique(articles$Biomeasures)
           ),
    ),
    column(2,
           checkboxGroupInput(
             inputId = "Collection",
             label = "Collection",
             choices = unique(articles$Collection),
             selected = unique(articles$Collection)
           )
    ),
    column(2,
           checkboxGroupInput(
             inputId = "freq_feedback",
             label = "Frequency of feedback",
             choices = unique(articles$`Frequency of feedback`),
             selected = unique(articles$`Frequency of feedback`)
           )
    ),
    column(2,
           checkboxGroupInput(
             inputId = "Communication",
             label = "Communication",
             choices = unique(articles$Communication),
             selected = unique(articles$Communication)
           )
    ),
    column(2,
           checkboxGroupInput(
             inputId = "Behaviors",
             label = "Behaviors",
             choices = unique(articles$Behaviors),
             selected = unique(articles$Behaviors)
           )
    )
  ),
  fluidRow(
    column(2,
           checkboxGroupInput(
             inputId = "Outcome",
             label = "Outcome",
             choices = unique(articles$Outcome),
             selected = unique(articles$Outcome)
           )
    )
  ),
  fluidRow(
    actionButton("refresh", "Refresh Plot")
  ),
  fluidRow(
    plotOutput("sankey")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  #reactive expression to filter data based on checkbox input
  sankey_data <- reactive({
    articles %>%
      filter(
        Domain %in% input$Domain,
        Biomeasures %in% input$Biomeasures,
        Collection %in% input$Collection,
        `Frequency of feedback` %in% input$freq_feedback,
        Communication %in% input$Communication,
        Behaviors %in% input$Behaviors,
        Outcome %in% input$Outcome
      ) %>% 
      ggsankey::make_long(
        Domain,
        Biomeasures,
        Collection,
        `Frequency of feedback`,
        Communication,
        Behaviors,
        Outcome
      )
  })
  
  #render the plot
  output$sankey <- renderPlot({
    #Take a dependency on the refresh button
    input$refresh
    
    #use isolate() so the plot only updates when the button is clicked, not when
    #sankey_data is updated
    ggplot(isolate(sankey_data()),
           aes(x = x, 
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
  })
  
}

shinyApp(ui, server)