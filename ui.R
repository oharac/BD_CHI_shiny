
##################################
###           Header           ###
##################################
header <- dashboardHeader(
  title = HTML('Visualizing human impacts on at-risk marine biodiversity'), 
  disable = FALSE, 
  titleWidth  = 600,
  ### tabs for: link to the article, link to Casey's page, link to Ian's page?
  tags$li(a(href = 'https://brenniedev.github.io/ianbrunjes/',
            icon('user-circle'),
            title = 'Ian Brunjes'),
            class = 'dropdown'),
  tags$li(a(href = 'https://github.com/BrennieDev/OHI_shiny_app',
            icon('github'),
            title = 'GitHub'),
            class = 'dropdown'),
  tags$li(a(href = 'https://www.instagram.com/ciscodoggodisco/',
            icon('dog'),
            title = 'Here is my dog.'),
            class = 'dropdown')
 )




##################################
###          Sidebar           ###
##################################
sidebar <- dashboardSidebar( 
  width = 250,
  sidebarMenu(
    id = 'sidebar',
    style = 'position: relative; overflow: visible;',

    menuItem('About', tabName = 'about', icon = icon('question-circle')),
    menuItem('Impact by taxon', tabName = 'taxa_info', icon = icon('fish')),
    menuItem('Impact by stressor', tabName = 'str_info', icon = icon('ship')),
    menuItem('Impact by year', tabName = 'annual_impact', icon = icon('globe-americas')),
    menuItem('Intensification', tabName = 'global_trend', icon = icon('chart-line')),
    
    hr(),
    
    p('This Shiny app was developed in March 2021 by', 
      a('Ian Brunjes', href='https://brenniedev.github.io/ianbrunjes/', target = '_blank'),
      'and', a('Casey O\'Hara', href='http://www.oharascience.com/', target = '_blank'),
      '. Find the original paper at:'),
    p(a('O\'Hara, C. C., M. Frazier, B. S. Halpern, At-risk marine biodiversity faces extensive, expanding, and intensifying 
              human impacts.',
      em('Science'), '(2021). doi:10.1126/science.abe6731.', 
      href='https://doi.org/10.1126/science.abe6731', target = '_blank'))
    )
  )

##################################
###            Body            ###
##################################
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    tags$script('document.title = "CHI Dashboard"'),
    includeCSS(here('www', 'shiny_styles.css')),
    ),
  
  ## 3.1 Dashboard body -------
  tabItems(
    ###################################
    ###  About - Abstract and data  ###
    ###################################
    tabItem( tabName = 'about',
     div(id = 'temp_message_about',
         h1('Visualizing human impacts to at-risk marine biodiversity',
            style = 'color:white' , align = 'center'),
         tags$hr(),
         p('A Shiny App to explore data from'),
         h4('O\'Hara, C. C., M. Frazier, B. S. Halpern,',
            a('At-risk marine biodiversity faces extensive, expanding, and intensifying 
              human impacts.', href='https://doi.org/10.1126/science.abe6731'),
            em('Science'), '(2021).', 
            a('doi:10.1126/science.abe6731', href='https://doi.org/10.1126/science.abe6731',
              target = '_blank'), '.'),
         h3('Abstract'),
         p('Human activities and climate change threaten marine biodiversity worldwide, 
         though sensitivity to these stressors varies considerably by species and 
         taxonomic group. Mapping the spatial distribution of 14 anthropogenic stressors 
         from 2003 to 2013 onto the ranges of 1271 at-risk marine species sensitive 
         to them, we found that, on average, species faced potential impacts across 57% 
         of their ranges, that this footprint expanded over time, and that the impacts 
         intensified across 37% of their ranges. Although fishing activity dominated the 
         footprint of impacts in national waters, climate stressors drove the expansion 
         and intensification of impacts. Mitigating impacts on at-risk biodiversity is
         critical to supporting resilient marine ecosystems, and identifying the 
         cooccurrence of impacts across multiple taxonomic groups highlights 
         opportunities to amplify the benefits of conservation management.'),
       # hr(),
       # p('The fourteen individual stressors include:'),
       # tableOutput('stressor_info'),
       h3('Methods in brief'),
       p('For each of 1271 threatened and near-threatened 
          marine species comprehensively assessed and mapped for the IUCN Red List of 
          Threatened Species ("at-risk species"), we identified sensitivity to 14 
          anthropogenic stressors. We then intersected species range maps with relevant 
          maps of annual stressor intensity from 2003 to 2013 to determine the extent of 
          potential impacts across species’ ranges, as well as how rapidly these impacts 
          have been expanding in extent and increasing in intensity.'),
       p('For this dashboard, high resolution spatial data from the original paper 
          (~10 km x ~10 km in Mollweide equal-area coordinate reference system), 
          has been reprojected and aggregated to lower resolution for use in 
          in spatial visualizations.'),
       # hr(),
       h3('Data Sources'),
       p('O\'Hara, C. C., M. Frazier, B. S. Halpern, At-risk marine biodiversity faces extensive, expanding, 
          and intensifying human impacts.', em('Science'), '(2021).', 
         a('doi:10.1126/science.abe6731', href='https://doi.org/10.1126/science.abe6731',
           target = '_blank'), '.'),
       p('Code and results from data analysis for: C. C. O\'Hara, M. Frazier, 
          B. S. Halpern, At-risk marine biodiversity faces extensive, expanding, 
          and intensifying human impacts.', em('Knowledge Network for Biocomplexity'),  '(2020);', 
          a('doi:10.5063/SJ1J03', href = 'https://doi.org/10.5063/SJ1J03',
            target = '_blank'), '.')
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
                p('Intensification and abatement of impacts on at-risk biodiversity.',
                  strong('Intensification'), 'indicates the proportion (%) of species in a 
                   cell experiencing one or more stressors increasing in intensity;',
                  strong('abatement'), 'indicates proportion of species experiencing one 
                   or more stressors decreasing in intensity.', strong('Net intensification'),
                  'is the difference: (% intensifying - % abating).  Spine length 
                   indicates relative species richness (number of at-risk species).  
                   Color indicates % intensification/abatement: green = 100% abatement, 
                   magenta = 100% intensification, grey = 0%.')
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