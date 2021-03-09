## Load packages
library(here)

## Source files
source(here("R", "globeOutput.R"))
source(here("R", "habitatsOutput.R"))
source(here("R", "trendOutput.R"))

## Server code
server <- function(input, output) {
  # Read in data once
  annual_impact <- read_csv(here("data", "cumulative_impacts_annual.csv"))
  global_trends <- read_csv(here("data", "global_impact_trends.csv"))
  habitat_data <- read_csv(here("data", "habitat_impacts.csv"))
  
  # Set initial global values
  current_habitat <<- "rocky_reef"
  current_stressor <<- "shipping"
  globePlot <<- NULL
  
  # Habitat animated gif output
  output$habitatsPlot <- renderImage({
    buildHabitatsOutput(habitat_data, input$selected_habitat, current_habitat)
  }, deleteFile = TRUE)
  
  # Annual impact globe output
  output$globePlot <- renderGlobe({
    buildGlobeOutput(annual_impact, input$selected_year)
    })
  
  # Interactive trend map output
  output$trendPlotly <- renderPlotly({
    buildTrendMapOutput(global_trends, input$land_toggle)
    })
  
  # Trend boxplot output
  output$trendPlot <- renderPlot({
    buildTrendBoxOutput(global_trends, event_data("plotly_selected"))
    })
  
  # Stressor animated gif output
  output$stressorsPlot <- renderImage({
    buildStressorsOutput(habitat_data, input$selected_stressor, current_stressor)
    }, deleteFile = TRUE)
  
  # Caption for globe output
  output$globeCaption <- renderText({
    buildGlobeCaption(input$selected_year)
    })
  
  # Caption for trend output
  output$trendCaption <- renderText({
    buildTrendCaption(event_data("plotly_selected"))
    })
  
  # Caption for habitat output
  output$habitatCaption <- renderText({
    buildHabitatCaption(current_habitat)
    })
  
  # Caption for stressor output
  output$stressorCaption <- renderText({
    buildStressorCaption(current_stressor)
    })
  
  output$stressor_info <- renderTable({
    individual_impacts_df
  })
}