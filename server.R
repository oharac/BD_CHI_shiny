
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
  

  #####################################
  ###  Intensification map and box  ###
  #####################################
  
  ### get proper data from intens_r_list
  intens_df_reactive <- reactive({
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
    df <- df %>%
      filter(y > -87 & y < 87)
    return(df)
  }) %>%
    bindCache(input$intens_type)
  
  ### Interactive trend map output
  intens_pal_reactive <- reactive({
    ### diverging color palette from ColorBrewer
    incr_pal <- colorRampPalette(colors = c('#f7f7f7', '#8e0152'))(101)
    decr_pal <- colorRampPalette(colors = c('#f7f7f7', '#276419'))(101)
    zero_col <- 'grey90'
    pal_prms <- if(input$intens_type == 'net') {
      list(colors = c(rev(decr_pal), incr_pal[-1]),
           breaks = seq(-100, 100, 50))
    } else if(input$intens_type == 'incr') {
      list(colors = incr_pal,
           breaks = seq(0, 100, 25))
    } else if(input$intens_type == 'decr') {
      list(colors = decr_pal,
           breaks = seq(0, 100, 25))
    }
    return(pal_prms)
  })
  output$trendPlotly <- renderPlotly({
    pal_prms <- intens_pal_reactive()

    plot <- ggplot(data = intens_df_reactive()) +
      geom_raster(aes(x=x, y = y, fill = trend)) +
      theme_void() +
      theme(panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            panel.grid.major = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), 'cm'),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")) +
      labs(fill = NA) +
      scale_fill_gradientn(colors = pal_prms$colors,
                           breaks = pal_prms$breaks,
                           labels = paste0(pal_prms$breaks, '%'),
                           na.value = 'grey80') +
      coord_fixed(ratio = 1, xlim = c(-180, 180), ylim = c(-87, 87)) 
        ### to set aspect ratio.  Clip northern and southernmost cells
      
    # Output as interactive plotly
    ggplotly(plot)
  }) %>%
    bindCache(input$intens_type)

  
  intens_select_reactive <- reactive({
    selected_points <- event_data('plotly_selected')
    if(!is.null(selected_points)) {
      # Bump selected points to match indexes
      selected_rows <- selected_points$pointNumber + 1
      
      # Filter on selected points
      selected_data <- intens_df_reactive() %>%
        filter(row_number() %in% selected_rows)
      return(selected_data)
    } else {
      return(intens_df_reactive())
    }
  })

  ### Trend boxplot output
  output$trendPlot <- renderPlot({
    global <- intens_df_reactive()
    selected <- intens_select_reactive()
    # Rescale value for mean of selected points
    min <- min(global$trend)
    max <- max(global$trend)
    mean <- mean(selected$trend)
    mean_scaled <- ceiling((mean-min)/(max-min) * 100)

    # Derive color from continuous palette for rescaled mean
    col <- intens_pal_reactive()[mean_scaled]

    # Build boxplot
    ggplot(data = selected, aes(x = trend)) +
      geom_boxplot(color = "white", fill= 'blue', size = 1.5) +
      plot_theme +
      xlim(min, max)
  })

  ### Caption for trend output
  output$trendCaption <- renderText({
    if (!is.null(event_data('plotly_selected'))) {
      "Distribution of intensification/abatement for selected area."
    } else {
      "Distribution of intensification/abatement globally."
    }
  })
  
}