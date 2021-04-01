
##################################
###           Header           ###
##################################
header <- dashboardHeader(
  title = tags$p(
            tags$a(
              tags$img(src='img/science_logo.jpg', height = 43), 
              ('Visualizing human impacts on at-risk marine biodiversity'),
              href = 'https://doi.org/10.1126/science.abe6731', target = '_blank'),
            align = 'left'),
  disable = FALSE, 
  titleWidth  = '100%'
 )


##################################
###          Sidebar           ###
##################################
sidebar <- dashboardSidebar( 
  width = 250,
  sidebarMenu(
    id = 'sidebar',
    style = 'position: relative; overflow: visible;',

    menuItem('Home', tabName = 'home', icon = icon('home')),
    menuItem('About this project', tabName = 'about', icon = icon('question-circle')),
    menuItem('Global impact map', tabName = 'annual_impact', icon = icon('globe-americas')),
    menuItem('Global expansion map', tabName = 'expansion', icon = icon('globe-africa')),
    menuItem('Global intensification map', tabName = 'intensification', icon = icon('globe-asia')),
    menuItem('Impact by taxon', tabName = 'taxa_info', icon = icon('fish')),
    menuItem('Impact by stressor', tabName = 'str_info', icon = icon('ship')),
    
    hr(),
    
    div(id = 'sidebar_note',
        h2('Visualizing human impacts on at-risk marine biodiversity',
           style = 'color:white' , align = 'left'),
        tags$hr(),
        p('A Shiny App to explore data from'),
        h4('O\'Hara, C. C., M. Frazier, B. S. Halpern,',
           a('At-risk marine biodiversity faces extensive, expanding, and intensifying 
              human impacts.', href='https://doi.org/10.1126/science.abe6731'),
           em('Science'), '(2021).', 
           a('doi:10.1126/science.abe6731', href='https://doi.org/10.1126/science.abe6731',
             target = '_blank'), '.')
    ),
    ### problematic sidebar credits:
    column(2,
           p('This Shiny app was developed in March 2021 by',
             a('Ian Brunjes', href='https://brenniedev.github.io/ianbrunjes/', target = '_blank'),
             'and', a('Casey O\'Hara.', href='http://www.oharascience.com/', target = '_blank'),
             'Find the original paper at:'),
           p(a('O\'Hara, C. C., M. Frazier, B. S. Halpern, At-risk marine biodiversity faces extensive,', br(), 'expanding, and intensifying
              human impacts.',
               em('Science'), '(2021). doi:10.1126/science.abe6731.',
               href='https://doi.org/10.1126/science.abe6731', target = '_blank'))
    )
  )
)

##################################
###            Body            ###
##################################
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    tags$script('document.title = "Dashboard: Visualizing human impacts on at-risk marine biodiversity"'),
    includeCSS(here('www', 'shiny_styles.css')),
    ),
  
  ## 3.1 Dashboard body -------
  tabItems(
    ########################################
    ###  Home - map and very basic info  ###
    ########################################
    tabItem( tabName = 'home',
             div(id = 'home',
                 h2('Visualizing human impacts on at-risk marine biodiversity',
                    style = 'color:white' , align = 'left'),
                 tags$hr(),
                 p('A Shiny App to explore data from'),
                 h4('O\'Hara, C. C., M. Frazier, B. S. Halpern,',
                    a('At-risk marine biodiversity faces extensive, expanding, and intensifying 
              human impacts.', href='https://doi.org/10.1126/science.abe6731'),
                    em('Science'), '(2021).', 
                    a('doi:10.1126/science.abe6731', href='https://doi.org/10.1126/science.abe6731',
                      target = '_blank'), '.')
             ),
             img(src = 'img/fig2b_for_shiny.png', align = 'center')
             
    ),
    
    ###################################
    ###  About - Abstract and data  ###
    ###################################
    tabItem( tabName = 'about',
     div(id = 'temp_message_about',
         h2('About this project',
            style = 'color:white' , align = 'left'),
         tags$hr(),
         radioGroupButtons(
           inputId = 'about_select',
           label = NULL,
           choices = c('Abstract' = 'abstract', 
                       'Methods in brief' = 'methods', 
                       'Stressors' = 'stressors',
                       # 'Species',
                       'Data' = 'data'),
           justified = TRUE,
           selected = 'abstract'
         ),
         htmlOutput('about')
       ),
     ),
    
    #################################
    ###  Impact by taxon boxplot  ###
    #################################
    tabItem(tabName = 'taxa_info',
            div(id = 'temp_message_about',
                h2('Impacted range across stressors, by taxon',
                   style = 'color:white' , align = 'left' ) ,
                tags$hr()
             ),
             column(12,
                    selectInput('selected_taxon', label = 'By Taxon:', 
                                choices  = taxa_names$tx_name,
                                selected = 1),
                    textOutput('taxonCaption'),
                    plotOutput('taxonPlot')
             )
    ),
    
    ##################################
    ### Impact by stressor boxplot ###
    ##################################
    tabItem(tabName = 'str_info',
            div(id = 'temp_message_about',
                h2('Impacted range across taxa, by stressor',
                   style = 'color:white' , align = 'left' ) ,
                tags$hr()
             ),
            column(12,
                   selectInput('selected_stressor', label = 'By Stressor:',
                               choices = str_names$str_name,
                               selected = 1),
                   textOutput('strCaption'),
                   plotOutput('strPlot')
            )
    ),
    
    ####################################
    ###     Impact globe output      ###
    ####################################
    tabItem(tabName = 'annual_impact',
            div(id = 'annual_trend',
              h2('Cumulative impact on at-risk marine species',
                 style = 'color:white' , align = 'left' ) ,
              tags$hr(),
              p('Cumulative impact on at-risk biodiversity by stressor category
                 (2013 data shown). Spine length indicates relative species richness 
                 (number of at-risk species) in that location.  Color indicates 
                 proportion of species affected by one or more stressors in that 
                 location (purple = 0%, yellow = 100%).')
            ),
     
            # radioGroupButtons(
            #   inputId = 'impact_year',
            #   label = 'Select year:',
            #   choices = c(2003:2013),
            #   justified = TRUE,
            #   selected = 2008
            #   ),
            radioGroupButtons(
              inputId = 'impact_cat',
              label = 'Select stressor category:',
              choices = c('Cumulative' = 'all', 
                          'Fishing'    = 'fishing', 
                          'Climate'    = 'climate',
                          'Land-based' = 'land-based',
                          'Ocean-based' = 'ocean'),
              justified = TRUE,
              selected = 'all'
            ),
            
           globeOutput('impactsGlobe', height = '450px'),
           
           
     ),
    
    ####################################
    ###    Expansion globe output    ###
    ####################################
    tabItem(tabName = 'expansion',
            div(id = 'expansion',
                h2('Expansion of impacts from 2003 to 2013',
                   style = 'color:white' , align = 'left' ) ,
                tags$hr(),
                p('Increase in proportion of impacted species due to expansion
                   of stressor footprint. Spine length indicates relative species richness 
                 (number of at-risk species) in that location.  Color indicates 
                 change in proportion of species affected by one or more stressors in that 
                 location: green = 100% -> 0%, magenta = 0% -> 100%, grey = no change.')
            ),
            
            # radioGroupButtons(
            #   inputId = 'impact_year',
            #   label = 'Select year:',
            #   choices = c(2003:2013),
            #   justified = TRUE,
            #   selected = 2008
            #   ),
            radioGroupButtons(
              inputId = 'expand_cat',
              label = 'Select stressor category:',
              choices = c('Cumulative' = 'all', 
                          'Fishing'    = 'fishing', 
                          'Climate'    = 'climate',
                          'Land-based' = 'land-based',
                          'Ocean-based' = 'ocean'),
              justified = TRUE,
              selected = 'all'
            ),
            
            globeOutput('expandGlobe', height = '450px'),
            p('Note that the change in species affected by the cumulative
               set of stressors may appear smaller in certain places than the
               change in species affected by a subset of stressors.  This occurs
               because stressors may be expanding onto ranges of species already 
               affected by another category of stressors.  For example, if a
               species was already impacted by fishing stressors in 2003, and 
               climate stressors expand into the same area by 2013, the cumulative
               set of stressors will show no change even though the climate
               stressors will show an increase.')
    ),
    
    ######################################
    ###  Intensification globe output  ###
    ######################################
    tabItem(tabName = 'intensification',
            div(id = 'intensification',
                h2('Impact intensification',
                   style = 'color:white' , align = 'left' ) ,
                tags$hr(),
                p('Intensification and abatement of impacts on at-risk biodiversity.
                   Spine length indicates relative species richness (number of at-risk 
                   species).  Color indicates % intensification/abatement: 
                   green = 100% abatement, magenta = 100% intensification, grey = 0%.')
            ),
            
            radioGroupButtons(
              inputId = 'intens_cat',
              label = 'Select stressor category:',
              choices = c('Cumulative' = 'all', 
                          'Fishing'    = 'fishing', 
                          'Climate'    = 'climate',
                          'Land-based' = 'land-based',
                          'Ocean-based' = 'ocean'),
              justified = TRUE,
              selected = 'all'
            ),

            globeOutput('intensGlobe', height = '450px'),
            
            radioGroupButtons(
              inputId = 'intens_type',
              label = NULL,
              choices = c('net % intensifying' = 'net',
                          '% intensifying' = 'incr', 
                          '% abating' = 'decr'),
              justified = TRUE,
              selected = 'net'
            ),
            p(strong('Intensification'), 'indicates the proportion (%) of species in a 
              cell experiencing one or more stressors increasing in intensity;',
                  strong('abatement'), 'indicates proportion of species experiencing one 
              or more stressors decreasing in intensity.', strong('Net intensification'),
                  'is the difference: (% intensifying - % abating).')
    )
  )
)

## Assemble UI components as dashboard page -------------
ui <- dashboardPage(header, sidebar, body, skin = 'blue')