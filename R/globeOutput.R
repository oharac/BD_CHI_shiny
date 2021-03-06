## Load packages
library(threejs)
library(tidyverse)

## Build 3d globe output
buildGlobeOutput <- function(impact_data, year_input){
  selected_year <- paste("cumulative_impact_", as.character(year_input), sep = "") 
  
  # Quartile of each value for color assignment
  impact_data$q <- as.numeric(
    cut(impact_data[[selected_year]],
      breaks=quantile(impact_data[[selected_year]], probs=c(0,0.25,0.5,0.75,1)),
      include.lowest=TRUE))
  # Assign colors for each level
  col = c("#0055ff","#00aaff","#00ffaa","#aaff00")[impact_data$q]
  
  # Globejs to build 3d globe
  globejs(lat=impact_data$lat,
   long=impact_data$long,
   val=impact_data[[selected_year]] * 20,
   pointsize=1,
   color = col,
   atmosphere=TRUE,
   title = "2002")
}

## Build caption for 3d globe canvas
buildGlobeCaption <- function(year) {
  paste("Cumulative Human Impact assessed for the year ", year, ".", sep = "")
}