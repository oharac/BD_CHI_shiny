source(here('R', 'setup.R'))
source(here('R', 'load_data.R'))

### Server code
server <- function(input, output) {
  
  #################################
  ###  Impact by taxon boxplot  ###
  #################################
  
  taxa_impacts <- reactive({
    ### why is 'selected' argument not working in ui?
    tx <- input$selected_taxon
    taxa_impacts <- impact_df %>% 
      filter(desc == tx | tx == 'all at-risk species') %>%
      left_join(str_names, by = c('stressor' = 'str_field')) %>%
      mutate(str_name = fct_rev(str_name))
    return(taxa_impacts)
  })
  
  output$taxonPlot <- renderPlot({
    means_df <- taxa_impacts() %>%
      group_by(str_name) %>%
      summarize(impact_pct = mean(impact_pct))
    
    taxa_plot <- ggplot(taxa_impacts(), aes(x = str_name, y = impact_pct)) +
      # ggplot(taxa_impacts) +
      geom_boxplot(aes(fill = category), 
                   color = 'grey90') +
      geom_point(data = means_df, shape = 23, size = 3,
                 color = 'grey90', fill = 'red') +
      scale_fill_viridis_d() +
      coord_flip() +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.position = 'none',
            plot.background  = element_rect(fill = 'black', color = NA),
            panel.background = element_rect(fill = 'black', color = NA),
            axis.text = element_text(color = 'white', size = 12))
    return(taxa_plot)
  })

  output$taxonCaption <- renderText({
    sprintf('Boxplot shows distribution of impacted range for %s (n = %s),
             by stressor and stressor category. Red diamond indicates
             mean value.', input$selected_taxon, n_distinct(taxa_impacts()$iucn_sid))
  })
  
  ##################################
  ### Impact by stressor boxplot ###
  ##################################
  
  str_impacts <- reactive({
    ### why is 'selected' argument not working in ui?
    str <- input$selected_stressor # str <- 'Cumulative (all stressors)'
    str_impacts <- impact_df %>% 
      left_join(str_names, by = c('stressor' = 'str_field')) %>%
      filter(str_name == str)
    str_order <- str_impacts %>%
      group_by(desc) %>%
      summarize(m = mean(impact_pct)) %>%
      arrange(m) %>%
      .$desc
    
    str_all <- str_impacts %>%
      mutate(desc = 'all species')
    
    str_impacts <- bind_rows(str_all, str_impacts) %>%
      mutate(desc = factor(desc, levels = c(str_order, 'all species')))
    
    return(str_impacts)
  })
  
  output$strPlot <- renderPlot({
    means_df <- str_impacts() %>%
      group_by(desc) %>%
      summarize(impact_pct = mean(impact_pct))
    
    str_plot <- ggplot(str_impacts(), aes(x = desc, y = impact_pct)) +
      geom_boxplot(aes(fill = category), 
                   color = 'grey90') +
      geom_point(data = means_df, shape = 23, size = 3,
                 color = 'grey90', fill = 'red') +
      scale_fill_viridis_d() +
      coord_flip() +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.position = 'none',
            plot.background  = element_rect(fill = 'black', color = NA),
            panel.background = element_rect(fill = 'black', color = NA),
            axis.text = element_text(color = 'white', size = 12))
    return(str_plot)
  })
  
  output$strCaption <- renderText({
    sprintf('Boxplot shows distribution of impacted range by taxon for
             %s. Red diamond indicates mean value.', 
            tolower(input$selected_stressor))
  })
  
  ##################################
  ### Annual impact globe output ###
  ##################################
  
  map_year <- reactive({
    ### color vector for translating pct impacted into a viridis value
    ### index 1 corresponds to 0%, while index 101 corresponds to 100%
    colors_gradient <- hcl.colors(n = 101, palette = 'viridis')
    
    ### define a function for transforming species richness
    richness_xfm <- function(x) {
      x^.7 ### a little less drastic than sqrt
    }
    
    map_year <- map_year_list[[input$selected_year]] %>%
      ### NOTE: because R indexes from 1, add 1 to pct_impact for index
      mutate(col = colors_gradient[pct_imp + 1]) %>%
      mutate(length = richness_xfm(nspp))
    return(map_year)
  })
  
  output$globePlot <- renderGlobe({
    globePlot <<- globejs(
      lat  = map_year()$y,
      long = map_year()$x,
      val  = map_year()$length,
      pointsize = 1,
      color = map_year()$col,
      atmosphere = TRUE,
      title = input$selected_year)
    }) %>%
    bindCache(input$selected_year)
  
  ### Caption for globe output
  output$globeCaption <- renderText({
    sprintf("Cumulative impact on at-risk biodiversity for %s. Length indicates 
             relative species richness (number of at-risk species).  Color 
             indicates proportion of species impacted 
             (purple = 0%%, yellow = 100%%).", input$selected_year)
  })
  
  
  ### Interactive trend map output
  # output$trendPlotly <- renderPlotly({
  #   buildTrendMapOutput(global_trends, input$land_toggle)
  #   })
  
  ### Trend boxplot output
  # output$trendPlot <- renderPlot({
  #   buildTrendBoxOutput(global_trends, event_data('plotly_selected'))
  #   })
  
  
  
  ### Caption for trend output
  # output$trendCaption <- renderText({
  #   buildTrendCaption(event_data('plotly_selected'))
  #   })
  

  output$stressor_info <- renderTable({
    str_table
    })
}