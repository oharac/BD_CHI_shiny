
### Server code
server <- function(input, output) {
  
  #################################
  ###      Stressor table       ###
  #################################
  output$stressor_info <- renderTable({
    str_table
  })
  
  #################################
  ###  Impact by taxon boxplot  ###
  #################################
  
  taxa_impacts <- reactive({
    tx <- input$selected_taxon
    taxa_impacts <- impact_df %>% 
      filter(desc == tx | tx == 'all at-risk species') %>%
      left_join(str_names, by = c('stressor' = 'str_field')) %>%
      mutate(str_name = fct_rev(str_name))
    return(taxa_impacts)
  }) %>%
    bindCache(input$selected_taxon)
  
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
  }) %>%
    bindCache(input$selected_taxon)

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
  }) %>%
    bindCache(input$selected_stressor)
  
  output$strPlot <- renderPlot({
    means_df <- str_impacts() %>%
      group_by(desc) %>%
      summarize(impact_pct = mean(impact_pct))
    
    str_plot <- ggplot(str_impacts(), aes(x = desc, y = impact_pct)) +
      geom_boxplot(aes(fill = desc), 
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
  }) %>%
    bindCache(input$selected_stressor)
  
  output$strCaption <- renderText({
    sprintf('Boxplot shows distribution of impacted range by taxon for
             %s. Red diamond indicates mean value.', 
            tolower(input$selected_stressor))
  })
  
  ##################################
  ### Annual impact globe output ###
  ##################################
  
  impacts_year_map <- reactive({
    ### color vector for translating pct impacted into a viridis value
    ### index 1 corresponds to 0%, while index 101 corresponds to 100%
    colors_gradient <- hcl.colors(n = 101, palette = 'viridis')
    
    richness_xfm <- function(x) {
      x^.7 ### a little less drastic than sqrt
    }
    
    impacts_year_map <- map_year_list[[input$selected_year]] %>%
      ### NOTE: because R indexes from 1, add 1 to pct_impact for index
      mutate(col = colors_gradient[pct_imp + 1]) %>%
      mutate(length = richness_xfm(nspp))
    return(impacts_year_map)
  })
  
  output$impactsGlobe <- renderGlobe({
    globePlot <<- globejs(
      lat  = impacts_year_map()$y,
      long = impacts_year_map()$x,
      val  = impacts_year_map()$length,
      pointsize = 1,
      color = impacts_year_map()$col,
      atmosphere = TRUE,
      title = input$selected_year)
    }) %>%
    bindCache(input$selected_year)
  
  ### Caption for globe output
  output$impactsGlobeCaption <- renderText({
    sprintf('Cumulative impact on at-risk biodiversity for %s. Spine length indicates 
             relative species richness (number of at-risk species).  Color 
             indicates proportion of species affected by one or more stressors 
             (purple = 0%%, yellow = 100%%).', input$selected_year)
  })
  


  ####################################
  ### Intensification globe output ###
  ####################################
  
  intens_df_reactive <- reactive({
    # input <- list(intens_type = 'decr')
    type <- input$intens_type
    message('creating intens_df_reactive element, type = ', type)
    category <- 'all'
    i_stem <- '%s_%s'
    if(type == 'net') {
      ### subtract decr from incr to get net
      i1 <- sprintf(i_stem, category, 'incr')
      i2 <- sprintf(i_stem, category, 'decr')
      df <- intens_r_list[[i1]] %>%
        full_join(intens_r_list[[i2]] %>%
                    select(x, y, pct_decr = pct_int)) %>%
        mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
               trend = pct_int - pct_decr)
    } else {
      ### choose the proper version and just return it
      i <- sprintf(i_stem, category, type)
      df <- intens_r_list[[i]] %>%
        rename(trend = pct_int)
    }
    
    return(df)
  }) %>%
    bindCache(input$intens_type)
  
  intens_map_reactive <- reactive({
    ### color vector for translating pct intensification into a color
    ### note: for net, need to add 100 to the indices
    ### diverging color palette from ColorBrewer
    incr_pal <- colorRampPalette(colors = c('#f7f7f7', '#8e0152'))(101)
    decr_pal <- colorRampPalette(colors = c('#f7f7f7', '#276419'))(101)
    # input <- list(intens_type = 'decr')
    colors_gradient <- switch(input$intens_type,
                              net = c(rev(decr_pal), incr_pal[-1]),
                              incr = incr_pal,
                              decr = decr_pal)
    bump <- ifelse(input$intens_type == 'net', 101, 1)
    
    ### define a function for transforming species richness
    richness_xfm <- function(x) {
      x^.7 ### a little less drastic than sqrt
    }
    
    intens_map <- intens_df_reactive() %>%
      ### NOTE: because R indexes from 1, add 1 to pct_impact for index
      mutate(col = colors_gradient[trend + bump]) %>%
      mutate(length = richness_xfm(nspp))
    return(intens_map)
  })
  
  output$intensGlobe <- renderGlobe({
    globePlot <<- globejs(
      lat  = intens_map_reactive()$y,
      long = intens_map_reactive()$x,
      val  = intens_map_reactive()$length,
      pointsize = 1,
      color = intens_map_reactive()$col,
      atmosphere = TRUE,
      title = input$intens_type)
  }) %>%
    bindCache(input$intens_type)
  
  ### Caption for globe output
  output$intensGlobeCaption <- renderText({
    'Intensification and abatement of impacts on at-risk biodiversity. 
    "Intensification" indicates the proportion (%) of species in a cell 
    experiencing one or more stressors increasing in intensity; "abatement" 
    indicates proportion of species experiencing one or more stressors 
    decreasing in intensity. "Net intensification" is the difference:
    (% intensifying - % abating).  Spine length indicates relative species 
    richness (number of at-risk species).  Color indicates 
    % intensification/abatement: green = 100%% abatement, magenta = 100%% 
    intensification, white = 0%%.'
  })
  
}