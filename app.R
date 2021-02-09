####
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(withr)
library(shinyBS)
library(shinyjs)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")


## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
  dashboardHeader( title = HTML("Ocean Health Index (OHI) Score Visualizations"), 
                   disable = FALSE, 
                   titleWidth  = 550
                   
  )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='http://www.oceanhealthindex.org',
                                             tags$img(src='placeholder.png'),
                                             target = '_blank') #,height='67',width='228.6', align = 'left'



## 2. sidebar ------------------------------
siderbar <- 
  dashboardSidebar( 
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      #style = "position: relative; overflow: visible; overflow-y:scroll",
      #style = 'height: 90vh; overflow-y: auto;',
      ## 1st tab show the Main dashboard -----------
      menuItem( "About", tabName = 'about', icon = icon('question-circle'), badgeColor = "green" ),
      
      ## add conditional panel to show more
      # conditionalPanel( "input.sidebar === 'dashboard'",
      #                   actionButton("btn_show_more",
      #                                paste0(' Show more details'),
      #                                icon = icon('chevron-circle-down'),
      #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
      #                                ) 
      #                   ),
      
      ## 2nd Second tab shows the country/region level tab --------------
      menuItem("OHI Global Scores", tabName = 'global_scores', icon = icon('globe') ),
      
      ## Show panel only when Commodity intelligence sidebar is selected
      useShinyjs(),
      
      ## 3th tab Data source, definition , i.e., help ---------------
      menuItem( "Score Breakdowns", tabName = 'score_breakdowns', icon = icon('dashboard') )
    )
  )

## 3. body --------------------------------
body <- dashboardBody( 
  ## 3.0. CSS styles in header ----------------------------
  tags$head(
    # ## JS codes
    # tags$script(src = "fixedElement.js" ),
    # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
    #                  .scroller{background: white; 
    #                   border: 1px solid #CCC; 
    #                   margin:0 0 10px; 
    #                   z-index:100; 
    #                   height:50px; 
    #                   font-size:18px; 
    #                   font-weight:bold; 
    #                   text-align:center; 
    #                  width:500px;}")),
    
    #tags$script(src = "world.js" ),
    tags$script("document.title = 'New Zealand Trade Intelligence Dashboard'"),
    
    ### Styles 
    tags$style(HTML(".small-box {height: 65px}")),
    tags$style(HTML(".fa { font-size: 35px; }")),
    tags$style(HTML(".glyphicon { font-size: 33px; }")),  ## use glyphicon package
    tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
    tags$style(HTML(".fa-globe { font-size: 20px; }")),
    tags$style(HTML(".fa-barcode { font-size: 20px; }")),
    tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
    tags$style(HTML(".fa-wrench { font-size: 15px; }")),
    tags$style(HTML(".fa-refresh { font-size: 15px; }")),
    tags$style(HTML(".fa-search { font-size: 15px; }")),
    tags$style(HTML(".fa-comment { font-size: 20px; }")),
    tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
    tags$style(HTML(".fa-envelope { font-size: 20px; }")),
    tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
    tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
    tags$style(HTML(".fa-bell { font-size: 17px; }")),
    tags$style(HTML(".fa-check { font-size: 14px; }")),
    tags$style(HTML(".fa-times { font-size: 14px; }")),
    
    #tags$style(HTML(".fa-twitter { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-facebook { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-google-plus { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-pinterest-p { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-linkedin { font-size: 10px; color:red;}")),
    #tags$style(HTML(".fa-tumblr { font-size: 10px; color:red;}")),
    
    ## modify the dashboard's skin color
    tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
    ),
    
    ## modify icon size in the sub side bar menu
    tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      } 
                      '
    )) ,
    
    tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
    
    ## to not show error message in shiny
    tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
    tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
    
    ## heand dropdown menu size
    #tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu { width:100px;}'))
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
    tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}'))
  ),
  
  ## 3.1 Dashboard body --------------
  tabItems(
    ## 3.1.1 Main dashboard ----------------------------------------------------------
    tabItem( tabName = 'about',
             ## contents for the about tab
             div(id = 'temp_message_about',
                 h1('Information about project goes here',
                    style = "color:darkblue" , align = "center" ) ,
                 tags$hr(),
                 p("Ocean Health Index. 2020. ohi-global v2020.1: Ocean Health Index 2020 global assessment [date downloaded]. National Center for Ecological Analysis and Synthesis, University of California, Santa Barbara. Available at: https://github.com/OHI-Science/ohi-global"),
                 tags$hr()
             ),
             
             # a button
             actionButton("action", label = "Learn More...")
             
    ),
    
    ## 3.1.2 Global Scores ------------------------
    tabItem( tabName = 'global_scores',

             ## contents for the about tab
             div(id = 'temp_message_global_scores',
                 h1('Interactive Map Widget goes here',
                    style = "color:darkblue" , align = "center" ) ,
                 tags$hr()
             ),
             
             # a slider
             sliderInput("slider1", label = h3("Year"), min = 2010, 
                         max = 2020, value = 2020)
             
    ),
    
    ## 3.1.3 Score Breakdowns ------------------------
    tabItem( tabName = 'score_breakdowns',
             ## contents for the score breakdowns tab
             div(id = 'temp_message_about',
                 h1('Detailed scores by country go here',
                    style = "color:darkblue" , align = "center" ) ,
                 tags$hr()
             ),
             
             # a dropdown
             selectInput("select", label = h3("Country"), 
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                         selected = 1), 
    )
  )
)



## put UI together --------------------
ui <- dashboardPage(header, siderbar, body )


# Server code
server <- function(input, output) {}


# Build app
shinyApp(ui, server)