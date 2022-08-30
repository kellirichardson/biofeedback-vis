library(shiny)
library(shinyWidgets) #for selectizeGroup widget
library(shinycssloaders) #for loading indicator
library(tidyverse)
library(ggsankey)
library(plotly)
library(colorspace)

palKelly <- function(n) {
  pal <- c(
    # "#f2f3f4", "#222222", # white and black removed
    "#f3c300", "#875692", "#f38400", "#a1caf1", "#be0032", "#c2b280",
    "#848482", "#008856", "#e68fac", "#0067a5", "#f99379", "#604e97",
    "#f6a600", "#b3446c", "#dcd300", "#882d17", "#8db600", "#654522",
    "#e25822", "#2b3d26"
  )
  while(n > length(pal)) {
    pal <- c(pal, pal)
  }
  pal[1:n]
}

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
    plotlyOutput(
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
  output$sankey <- renderPlotly({
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
    
    #isolate these for constructing title
    start_yr <- isolate(input$year_range[1])
    end_yr <- isolate(input$year_range[2])

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
      #count number of unique references
      summarize(n_refs = length(unique(value))) %>%
      rename(name = node) %>% 
      mutate(node = 0:(n()-1)) %>% 
      mutate(color = palTableau20(n()))
    
    # Join together for links table, omit NA
    links <- 
      left_join(results_n, nodes, by = c("node" = "name")) %>%
      left_join(nodes, by = c("next_node" = "name")) %>%
      select(source = node.y, target = node.y.y,
             value = n, n_refs = n_refs.x, color = color.x) %>%
      na.omit()
    

# Plotly plot -------------------------------------------------------------
      plot_ly(
        type = "sankey",
        arrangement = "perpendicular", #keeps nodes in line.  Other possible options are "snap" or "fixed"

        #Define nodes
        node = list(
          label = nodes$name,
          customdata = nodes$n_refs,
          color = nodes$color,
          hovertemplate = "References: %{customdata:.d}<br>Observations: %{value:.d}<extra></extra>",

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
          color = adjust_transparency(links$color, 0.5),
          customdata = links$n_refs,
          hovertemplate = "References: %{customdata:.d}<br>Observations: %{value:.d}<extra></extra>"
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