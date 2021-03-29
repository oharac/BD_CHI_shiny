## Load packages
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(shinyWidgets)
library(tippy)

## 1. header -------------------------------
header <- dashboardHeader(
  title = HTML("Oceanic CHI"), 
  disable = FALSE, 
  titleWidth  = 350,
  tags$li(a(href = 'https://brenniedev.github.io/ianbrunjes/',
            icon("user-circle"),
            title = "Ian Brunjes"),
            class = "dropdown"),
  tags$li(a(href = 'https://github.com/BrennieDev/OHI_shiny_app',
            icon("github"),
            title = "GitHub"),
            class = "dropdown"),
  tags$li(a(href = 'https://www.instagram.com/ciscodoggodisco/',
            icon("dog"),
            title = "Here is my dog."),
            class = "dropdown")
 )




## 2. sidebar ------------------------------
sidebar <- dashboardSidebar( 
  width = 350,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",

    menuItem( "About", tabName = 'about', icon = icon('question-circle')),
    menuItem( "Impact Breakdowns", tabName = 'habitat_info', icon = icon('fish')),
    menuItem("Global Annual Impact", tabName = 'annual_impact', icon = icon('globe-americas')),
    menuItem( "Global Trends", tabName = 'global_trend', icon = icon('chart-line'))
    )
  )
## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    tags$script("document.title = 'CHI Dashboard'"),
    includeCSS(here("www", "shiny_styles.css")),
    ),
  
  ## 3.1 Dashboard body -------
  tabItems(
    ## 3.1.1 About ------------
    tabItem( tabName = 'about',
     div(id = 'temp_message_about',
       h1("Visualizing Cumulative Human Impact across the World's Ocean",
        style = "color:white" , align = "center" ),
       tags$hr(),
       h2("About the Data"),
       p("The dataset contains high resolution, annual data on the intensity of 14 human stressors and their impact on 21 marine ecosystems over 11 years (2003-2013)."),
       p("The fourteen individual stressors include:"),
       tableOutput("stressor_info"),
       p("Individual impacts for these stressors are assessed for every cell by multiplying their intensity with the vulnerability of present ecosystems to that stressor."),
       p("Cumulative Impact for each cell is the sum of all the individual impacts."),
       p("For this dashboard, the high resolution data has been reprojected, aggregated to lower resolution, and converted to data frames for use in spatial visualizations."),
       br(),
       tags$hr(),
       h2("Data Sources"),
       HTML("<p>Melanie Frazier. Recent pace of change in human impact on the world's ocean: Cumulative impacts. Knowledge Network for Biocomplexity. <a href='https://doi.org/10.5063/F12B8WBS'>https://doi.org/10.5063/F12B8WBS</a>.</p>"),
       HTML("<p>Halpern, B.S., Frazier, M., Afflerbach, J. et al. Recent pace of change in human impact on the world’s ocean. Sci Rep 9, 11609 (2019). <a href='https://doi.org/10.1038/s41598-019-47201-9'>https://doi.org/10.1038/s41598-019-47201-9</a>.</p>")
       ),
     ),
    
    ## 3.1.2 Habitat Breakdowns -------
    # tabItem( tabName = 'habitat_info',
    #          div(id = 'temp_message_about',
    #              h1('Breakdown of Impacts by Specific Habitats and Stressors',
    #                 style = "color:white" , align = "center" ) ,
    #              tags$hr()
    #          ),
    #          column(6,
    #                 selectInput("selected_habitat", label = "By Habitat:", choices = habitat_names, selected = "coral reef"),
    #                 imageOutput("habitatsPlot", height = "auto"),
    #                 textOutput("habitatCaption")
    #          ),
    #          column(6,
    #                 selectInput("selected_stressor", label = "By Stressor:", choices = stressor_names, selected = "shipping"),
    #                 imageOutput("stressorsPlot", height = "auto"),
    #                 textOutput("stressorCaption")
    #          )
    # ),
    
    ## 3.1.3 Global Scores ------------
    tabItem( tabName = 'annual_impact',
     div(id = 'annual_trend',
       h1('Global Cumulative Impact by Year',
        style = "color:white" , align = "center" ) ,
       tags$hr()
       ),
     
     radioGroupButtons(
       inputId = "selected_year",
       choices = c(2003:2013),
       justified = TRUE,
       selected = 2008
       ),
     
     globeOutput("globePlot"),
     textOutput("globeCaption")
     )# ,
    
    ## 3.1.4 Trends ------------------
    # tabItem( tabName = 'global_trend',
    #  div(id = 'temp_message_about',
    #    h1('Average Annual Change (Trend) of Cumulative Impact from 2003-2013',
    #     style = "color:white" , align = "center" ) ,
    #    tags$hr()
    #    ),
    #  
    #  fluidRow(
    #    column(2, offset = 2,
    #           fluidRow(
    #             materialSwitch(
    #               inputId = "land_toggle",
    #               label = HTML("<i id = 'trend-help' class='fa fa-question-circle' role='presentation' aria-label='question-circle icon'></i><span>Toggle Land Cover</span>"), 
    #               value = TRUE,
    #               status = "primary"
    #             ),
    #             tippy_this("trend-help", tooltip = "Use the switch to toggle land polygons, and use the interactive plot's select tool to refine a subset of data for the box plot below.", placement = "bottom")
    #           )
    #     )),
    #  
    #  fluidRow(
    #    column(8, offset = 2, plotlyOutput("trendPlotly"))
    #    ),
    #  
    #  plotOutput("trendPlot"),
    #  textOutput("trendCaption")
    #  )
    )
  )

## Assemble UI components as dashboard page -------------
ui <- dashboardPage(header, sidebar, body, skin = "blue")