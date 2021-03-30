

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
  width = 250,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",

    menuItem("About", tabName = 'about', icon = icon('question-circle')),
    menuItem("Impact by taxon", tabName = 'taxa_info', icon = icon('fish')),
    menuItem("Impact by stressor", tabName = 'str_info', icon = icon('ship')),
    menuItem("Global Annual Impact", tabName = 'annual_impact', icon = icon('globe-americas')),
    menuItem("Global Trends", tabName = 'global_trend', icon = icon('chart-line'))
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
       p("The dataset contains high resolution, annual data on the intensity 
          of 14 human stressors and their impact on 21 marine ecosystems over 
          11 years (2003-2013)."),
       p("The fourteen individual stressors include:"),
       tableOutput("stressor_info"),
       p("Individual impacts for these stressors are assessed for every cell 
          by multiplying their intensity with the vulnerability of present 
          ecosystems to that stressor."),
       p("Cumulative Impact for each cell is the sum of all the individual 
          impacts."),
       p("For this dashboard, the high resolution data has been reprojected, 
          aggregated to lower resolution, and converted to data frames for use 
          in spatial visualizations."),
       br(),
       tags$hr(),
       h2("Data Sources"),
       HTML("<p>Melanie Frazier. Recent pace of change in human impact on the 
             world's ocean: Cumulative impacts. Knowledge Network for 
             Biocomplexity. 
             <a href='https://doi.org/10.5063/F12B8WBS'>https://doi.org/10.5063/F12B8WBS</a>.</p>"),
       HTML("<p>Halpern, B.S., Frazier, M., Afflerbach, J. et al. Recent pace 
             of change in human impact on the world’s ocean. Sci Rep 9, 
             11609 (2019). 
             <a href='https://doi.org/10.1038/s41598-019-47201-9'>https://doi.org/10.1038/s41598-019-47201-9</a>.</p>")
       ),
     ),
    
    #################################
    ###  Impact by taxon boxplot  ###
    #################################
    tabItem(tabName = 'taxa_info',
            div(id = 'temp_message_about',
                h1('Impacted range across stressors, by taxon',
                   style = "color:white" , align = "center" ) ,
                tags$hr()
             ),
             column(12,
                    selectInput("selected_taxon", label = "By Taxon:", 
                                choices  = taxa_names$tx_name,
                                selected = 1),
                    textOutput("taxonCaption"),
                    plotOutput("taxonPlot")
             )
    ),
    
    ##################################
    ### Impact by stressor boxplot ###
    ##################################
    tabItem(tabName = 'str_info',
            div(id = 'temp_message_about',
                h1('Impacted range across taxa, by stressor',
                   style = "color:white" , align = "center" ) ,
                tags$hr()
             ),
            column(12,
                   selectInput("selected_stressor", label = "By Stressor:",
                               choices = str_names$str_name,
                               selected = 1),
                   textOutput("strCaption"),
                   plotOutput("strPlot")
            )
    ),
    
    ####################################
    ###  Annual impact globe output  ###
    ####################################
    tabItem(tabName = 'annual_impact',
            div(id = 'annual_trend',
              h1('Cumulative impact on at-risk marine species by year',
                 style = "color:white" , align = "center" ) ,
              tags$hr(),
              p('Cumulative impact on at-risk biodiversity by year and stressor
                 category. Spine length indicates relative species richness 
                 (number of at-risk species) in that location.  Color indicates 
                 proportion of species affected by one or more stressors in that 
                 location (purple = 0%, yellow = 100%).')
            ),
     
            radioGroupButtons(
              inputId = "impact_year",
              choices = c(2003:2013),
              justified = TRUE,
              selected = 2008
              ),
            radioButtons(
              inputId = "impact_cat",
              label = 'Stressor category',
              choices = c('Cumulative' = 'all', 
                          'Fishing'    = 'fishing', 
                          'Climate'    = 'climate',
                          'Land-based' = 'land-based',
                          'Ocean-based' = 'ocean'),
              inline = TRUE,
              selected = 'all'
            ),
            
           globeOutput("impactsGlobe")
     ),
    
    ######################################
    ###  Intensification globe output  ###
    ######################################
    tabItem(tabName = 'global_trend',
            div(id = 'annual_trend',
                h1('Intensification and abatement of impacts',
                   style = "color:white" , align = "center" ) ,
                tags$hr(),
                HTML('<p>Intensification and abatement of impacts on at-risk biodiversity.
                      <i>Intensification</i> indicates the proportion (%) of species in a 
                      cell experiencing one or more stressors increasing in intensity;
                      <i>abatement</i> indicates proportion of species experiencing one 
                      or more stressors decreasing in intensity. <i>Net intensification</i>
                      is the difference: (% intensifying - % abating).  Spine length 
                      indicates relative species richness (number of at-risk species).  
                      Color indicates % intensification/abatement: green = 100% abatement, 
                      magenta = 100% intensification, grey = 0%.</p>')
            ),
            
            radioButtons(
              inputId = "intens_type",
              label = 'Trend direction',
              choices = c('net % intensifying' = 'net',
                          '% intensifying' = 'incr', 
                          '% abating' = 'decr'),
              inline = TRUE,
              selected = 'net'
            ),
            radioButtons(
              inputId = "intens_cat",
              label = 'Stressor category',
              choices = c('Cumulative' = 'all', 
                          'Fishing'    = 'fishing', 
                          'Climate'    = 'climate',
                          'Land-based' = 'land-based',
                          'Ocean-based' = 'ocean'),
              inline = TRUE,
              selected = 'all'
            ),
            # radioGroupButtons(
            #   inputId = "intens_type",
            #   label = 'Trend direction',
            #   choices = c('% intensifying' = 'incr', 
            #               '% abating' = 'decr', 
            #               'net % intensifying' = 'net'),
            #   justified = TRUE,
            #   selected = 'net'
            # ),
            # radioGroupButtons(
            #   inputId = "intens_cat",
            #   label = 'Stressor category',
            #   choices = c('Cumulative' = 'all', 
            #               'Fishing'    = 'fishing', 
            #               'Climate'    = 'climate',
            #               'Land-based' = 'land-based',
            #               'Ocean-based' = 'ocean'),
            #   justified = TRUE,
            #   selected = 'all'
            # ),
            globeOutput("intensGlobe")
    )
  )
)

## Assemble UI components as dashboard page -------------
ui <- dashboardPage(header, sidebar, body, skin = "blue")