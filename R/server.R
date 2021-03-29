## Load packages
library(here)
library(tidyverse)

## Source files
source(here('R', 'setup.R'))
source(here('R', 'globeOutput.R'))
source(here('R', 'habitatsOutput.R'))
source(here('R', 'trendOutput.R'))

### Server code
server <- function(input, output) {
  
  ### Habitat animated gif output
  # output$habitatsPlot <- renderImage({
  #   buildHabitatsOutput(habitat_data, input$selected_habitat, current_habitat)
  # }, deleteFile = TRUE)
  
  ### Annual impact globe output
  output$globePlot <- renderGlobe({
    buildGlobeOutput(impact_list, input$selected_year)
    }) %>%
    bindCache(input$selected_year)
  
  ### Interactive trend map output
  # output$trendPlotly <- renderPlotly({
  #   buildTrendMapOutput(global_trends, input$land_toggle)
  #   })
  
  ### Trend boxplot output
  # output$trendPlot <- renderPlot({
  #   buildTrendBoxOutput(global_trends, event_data('plotly_selected'))
  #   })
  
  ### Stressor animated gif output
  # output$stressorsPlot <- renderImage({
  #   buildStressorsOutput(habitat_data, input$selected_stressor, current_stressor)
  #   }, deleteFile = TRUE)
  
  ### Caption for globe output
  output$globeCaption <- renderText({
    buildGlobeCaption(input$selected_year)
    })
  
  ### Caption for trend output
  # output$trendCaption <- renderText({
  #   buildTrendCaption(event_data('plotly_selected'))
  #   })
  
  ### Caption for habitat output
  # output$habitatCaption <- renderText({
  #   buildHabitatCaption(input$selected_habitat)
  #   })
  
  ### Caption for stressor output
  # output$stressorCaption <- renderText({
  #   buildStressorCaption(input$selected_stressor)
  #   })
  
  output$stressor_info <- renderTable({
    individual_impacts_df
  })
}