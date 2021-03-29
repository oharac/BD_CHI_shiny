## Load packages
library(threejs)

colors_gradient <- hcl.colors(n = 100, palette = 'viridis')
## Build 3d globe output
buildGlobeOutput <- function(impact_data, year_input) {
  ### color based on % impact, length based on species richness
  
  ### assign color based on percent of impact
  impact_data_year <- impact_data[[year_input]] %>%
    mutate(col = colors_gradient[pct_imp + 1])
      ### because R indexes from 1, pct_imp of 0 needs to be bumped up one etc.

  ### define a function for transforming species richness
  richness_xfm <- function(x) {
    x^.7
  }
  
  ### Globejs to build 3d globe
  globePlot <<- globejs(
    lat  = impact_data_year$y,
    long = impact_data_year$x,
    val  = richness_xfm(impact_data_year$nspp),
    pointsize = 1,
    color = impact_data_year$col,
    atmosphere = TRUE,
    title = year_input)
}

### Build caption for 3d globe canvas
buildGlobeCaption <- function(year) {
  sprintf("Cumulative Human Impact assessed for %s.", year)
}