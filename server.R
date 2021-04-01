
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
      scale_y_continuous(breaks = seq(0, 1, .25), labels = paste0(seq(0, 100, 25), '%')) +
      coord_flip() +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            legend.position = 'none',
            plot.background  = element_rect(fill = 'black', color = NA),
            panel.background = element_rect(fill = 'black', color = NA),
            axis.text = element_text(color = 'white', size = 12)) +
      labs(y = 'Percent of range affected')
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
      scale_y_continuous(breaks = seq(0, 1, .25), labels = paste0(seq(0, 100, 25), '%')) +
      coord_flip() +
      theme_classic() +
      theme(axis.title.x = element_blank(),
            legend.position = 'none',
            plot.background  = element_rect(fill = 'black', color = NA),
            panel.background = element_rect(fill = 'black', color = NA),
            axis.text = element_text(color = 'white', size = 12)) +
      labs(y = 'Percent of range affected')
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
    # input <- list(impact_cat = 'all')
    map_name <- paste(input$impact_cat, 2013, sep = '_')
    impacts_year_map <- impact_map_list[[map_name]] %>%
      ### NOTE: because R indexes from 1, add 1 to pct_impact for index
      mutate(col = colors_gradient[pct_imp + 1]) %>%
      mutate(length = richness_xfm(nspp))
    
    return(impacts_year_map)
  }) %>%
    bindCache(input$impact_cat)
  
  output$impactsGlobe <- renderGlobe({
    globePlot <- globejs(
      lat  = impacts_year_map()$y,
      long = impacts_year_map()$x,
      val  = impacts_year_map()$length,
      pointsize = 1,
      color = impacts_year_map()$col,
      atmosphere = TRUE,
      title = paste(2013, input$impact_cat))
    })

  ####################################
  ###    Expansion globe output    ###
  ####################################
  
  expand_pal <- function() {
    incr_pal <- colorRampPalette(colors = c('grey20', '#c51b7d'))(101)
    decr_pal <- colorRampPalette(colors = c('grey20', '#4d9221'))(101)
    colors_gradient <- c(rev(decr_pal), incr_pal[-1])
  }
  
  expand_map_reactive <- reactive({
    # input <- list(expand_cat = 'all')
    message('creating expand_map_reactives, category = ', input$expand_cat)
    
    expand_df <- impact_diff_list[[paste0('diff_', input$expand_cat)]]
    
    colors_gradient <- expand_pal()
    ### NOTE: because R indexes from 1, add 101 to diff for index so -100 -> 1
    bump <- 101
    
    expand_map <- expand_df %>%
      mutate(col = colors_gradient[delta + bump]) %>%
      mutate(length = richness_xfm(nspp))
    
    return(expand_map)
  })
  
  output$expandGlobe <- renderGlobe({
    globePlot <- globejs(
      lat  = expand_map_reactive()$y,
      long = expand_map_reactive()$x,
      val  = expand_map_reactive()$length,
      pointsize = 1,
      color = expand_map_reactive()$col,
      atmosphere = TRUE,
      title = paste('delta', input$expand_cat))
  })
  
  ####################################
  ### Intensification globe output ###
  ####################################
  
  intens_pal <- function(type) {
    ### color vector for translating pct intensification into a color
    incr_pal <- colorRampPalette(colors = c('grey20', '#c51b7d'))(101)
    decr_pal <- colorRampPalette(colors = c('grey20', '#4d9221'))(101)
    # input <- list(intens_type = 'decr')
    colors_gradient <- switch(type,
                              net = c(rev(decr_pal), incr_pal[-1]),
                              incr = incr_pal,
                              decr = decr_pal)
  }
  
  assemble_intens_df <- function(type, cat) {
    if(type == 'net') {
      ### subtract decr from incr to get net
      i1 <- sprintf('%s_%s', cat, 'incr')
      i2 <- sprintf('%s_%s', cat, 'decr')
      intens_df <- intens_r_list[[i1]] %>%
        full_join(intens_r_list[[i2]] %>%
                    select(x, y, pct_decr = pct_int)) %>%
        mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
               trend = pct_int - pct_decr)
    } else {
      ### choose the proper version and just return it
      i <- sprintf('%s_%s', cat, type)
      intens_df <- intens_r_list[[i]] %>%
        rename(trend = pct_int)
    }
  }
  
  intens_map_reactive <- reactive({
      # input <- list(intens_type = 'decr')
      message('creating intens_df_reactive element, type = ', input$intens_type, 
              ', category = ', input$intens_cat)
    
      intens_df <- assemble_intens_df(input$intens_type, input$intens_cat)

      colors_gradient <- intens_pal(input$intens_type)
      ### NOTE: because R indexes from 1, add 1 to pct_impact for index; if net,
      ### push -100 to become 1 etc.
      bump <- ifelse(input$intens_type == 'net', 101, 1)

      intens_map <- intens_df %>%
        mutate(col = colors_gradient[trend + bump]) %>%
        mutate(length = richness_xfm(nspp))

      return(intens_map)
    })
  
  output$intensGlobe <- renderGlobe({
    globePlot <- globejs(
      lat  = intens_map_reactive()$y,
      long = intens_map_reactive()$x,
      val  = intens_map_reactive()$length,
      pointsize = 1,
      color = intens_map_reactive()$col,
      atmosphere = TRUE,
      title = paste(input$intens_type, input$intens_cat))
  })
  
  ### Caption for globe output
  output$intensGlobeCaption <- renderText({
    'Intensification and abatement of impacts on at-risk biodiversity. 
    "Intensification" indicates the proportion (%) of species in a cell 
    experiencing one or more stressors increasing in intensity; "abatement" 
    indicates proportion of species experiencing one or more stressors 
    decreasing in intensity. "Net intensification" is the difference:
    (% intensifying - % abating).  Spine length indicates relative species 
    richness (number of at-risk species).  Color indicates 
    % intensification/abatement: green = 100% abatement, magenta = 100% 
    intensification, grey = 0%.'
  })
  
  ###################################
  ###  About - Abstract and data  ###
  ###################################
  about <- list(
    abstract = 'Human activities and climate change threaten marine biodiversity 
    worldwide, though sensitivity to these stressors varies considerably by 
    species and taxonomic group. Mapping the spatial distribution of 14 anthropogenic 
    stressors from 2003 to 2013 onto the ranges of 1271 at-risk marine species 
    sensitive to them, we found that, on average, species faced potential impacts 
    across 57% of their ranges, that this footprint expanded over time, and that 
    the impacts intensified across 37% of their ranges. Although fishing activity 
    dominated the footprint of impacts in national waters, climate stressors drove 
    the expansion and intensification of impacts. Mitigating impacts on at-risk 
    biodiversity is critical to supporting resilient marine ecosystems, and 
    identifying the cooccurrence of impacts across multiple taxonomic groups 
    highlights opportunities to amplify the benefits of conservation management.',
  methods = '<p>For each of 1271 threatened and near-threatened marine species 
    comprehensively assessed and mapped for the 
    <a href = "https://www.iucnredlist.org/" target = "_blank">IUCN Red List of 
    Threatened Species</a> ("at-risk species"), we identified sensitivity to 14 
    anthropogenic stressors.
    We then intersected species range maps with relevant maps of annual stressor 
    intensity from 2003 to 2013 to determine the extent of potential impacts 
    across species\' ranges, as well as how rapidly these impacts have been expanding
    in extent and increasing in intensity.</p>

    <p>For this dashboard, high resolution spatial data from the original paper 
    (~10 km x ~10 km in Mollweide equal-area coordinate reference system), 
    has been reprojected and aggregated to lower resolution for use in in 
    spatial visualizations.<p>',
  data = '<p>O\'Hara, C. C., M. Frazier, B. S. Halpern, At-risk marine 
    biodiversity faces extensive, expanding, and intensifying human impacts. 
    <i>Science</i>, (2021). 
    <a href = "https://doi.org/10.1126/science.abe6731" target = "_blank">
    doi:10.1126/science.abe6731</a>.</p>
  
    <p>Code and results from data analysis for: C. C. O\'Hara, M. Frazier, 
    B. S. Halpern, At-risk marine biodiversity faces extensive, expanding, 
    and intensifying human impacts. <i>Knowledge Network for Biocomplexity</i>, 
    (2020); <a href = "(https://doi.org/10.5063/SJ1J03)" target = "_blank">
    doi:10.5063/SJ1J03</a>.</p>',
  stressors = '<h3>Fishing stressors</h3>
    <ul><li><b>Fishing: artisanal:</b> Total tonnes of catch from nonindustrial fisheries calculated for each year (Watson 2018, 0.5° resolution). Catch divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by region’s productivity.
        <li><b>Fishing: demersal destructive:</b> Total tonnes of catch for industrial demersal fishing using gear types causing habitat destruction calculated for each year. Catch data divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by region’s productivity.
        <li><b>Fishing: demersal nondestructive high bycatch:</b> Total tonnes of catch for industrial demersal fishing using high bycatch practices calculated for each year. Catch data divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by region’s productivity.
        <li><b>Fishing: demersal nondestructive low bycatch:</b> Total tonnes of catch for industrial demersal fishing using low bycatch practices calculated for each year. Catch data divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by the region’s productivity.
        <li><b>Fishing: pelagic high bycatch:</b> Total tonnes of catch for industrial pelagic fishing using high bycatch practices calculated for each year. Catch data divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by region’s productivity.
        <li><b>Fishing: pelagic low bycatch:</b> Total tonnes of catch for industrial pelagic fishing using low bycatch practices calculated for each year. Catch data divided by corresponding year’s Net Primary Productivity (monthly data averaged for yearly NPP estimate) to standardize by region’s productivity.
    </ul>
    <h3>Climate stressors</h3>
    <ul><li><b>Sea surface temperature:</b> Number of extreme SST weeks during a 
        five-year period subtracted from the number of extreme SST weeks during a 
        baseline 5 year period (1985-1989). An extreme week is defined as (weekly 
        SST - weekly climatological SST) exceeding 1 SD of anomalies calculated 
        across 1982-2017 for that week.
        <li><b>Ocean acidification:</b> Monthly aragonite saturation values (Ω) 
        averaged to obtain annual estimates.
        <li><b>Sea level rise:</b> Monthly anomalies averaged to obtain annual 
        mean sea level anomaly. 5 year mean of annual data used to smooth large yearly variation.
    </ul>
    <h3>Land-based stressors</h3>
    <ul><li><b>Nutrient pollution (runoff):</b> Intensity of pollution from modeled
        plumes of land-based fertilizer pollution (Halpern et al. 2008), based 
        on country level fertilizer use (UN 2016), land cover data (Friedle et 
        al. 2010), and elevation data (USGS 2004)
        <li><b>Organic chemical pollution (runoff):</b> Intensity of pollution 
        from modeled plumes of land-based pesticide pollution (Halpern et al. 2008), 
        based on country level pesticide use (UN 2016), land cover data (Friedle et 
        al. 2010), and elevation data (USGS 2004)
        <li><b>Direct human disturbance:</b> Density (people per km<sup>2</sup>) 
        converted to population (people per raster cell). Intervening years 
        (i.e., years outside of 2000, 2005, 2010, 2015, 2020) interpolated using 
        a linear model. For each raster cell, coastal human population summed for 
        10 km radius. Data cropped to include only cells 1km from the coast.
        <li><b>Light pollution:</b> Non-calibrated radiance values from satellite 
        data calibrated across year/satellite following methods of Elvidge et al. 2009.
    </ul>
    <h3>Ocean-based stressors</h3>
    <ul><li><b>Shipping:</b> Tournadre (2018) data used to create yearly rasters 
      describing annual proportional change in shipping relative to 2011. 
      Multiplied yearly proportional change raster with high resolution 
      shipping raster (Halpern et al. 2015 and Wallbridge 2013) to estimate 
      shipping traffic over time.</ul>'
  )
  
  output$about <- renderText({
    about[[input$about_select]]
    })
  
  #################################
  ###    Calculating impacts    ###
  #################################
  output$calcMap <- renderImage({
    f <- normalizePath(
      file.path('./www', sprintf('maps/%s_%s.png', input$calc_step, input$calc_spp))
      )
    list(src = f)
  }, deleteFile = FALSE)
  output$calcCaption <- renderText({
    spp_info <- calc_spp_df %>%
      filter(iucn_sid == input$calc_spp) %>%
      mutate(comname = case_when(comname == 'Lettuce Coral' ~'Lettuce coral', 
                                 TRUE ~ paste('The', tolower(comname))))
    
    range_text <- sprintf('Global range for %s (%s, taxon: %s)', 
                          tolower(spp_info$comname), spp_info$sciname, spp_info$desc)
    str_ct_text <- sprintf('Stressors that threaten %s (%s): %s.  Map shows how many of 
                            these stressors are acting in any particular cell (2013 data).', 
                           tolower(spp_info$comname), spp_info$sciname, tolower(spp_info$strs))
    str_flat_text <- sprintf('Stressors, flattened to show presence of any stressor (%s) that threaten %s (%s)  (2013 data).', 
                             tolower(spp_info$strs), tolower(spp_info$comname), spp_info$sciname)
    impact_text   <- sprintf('"Impact" area indicates locations where range of %s (%s) overlaps one 
                             or more stressors that threaten it (%s).  "Refugia" are parts of the species 
                             range free from significant impacts; "no impact" areas indicate locations
                             where stressors act outside of the species range (2013 data).', 
                             tolower(spp_info$comname), spp_info$sciname, tolower(spp_info$strs))
    txt <- switch(input$calc_step,
                  range     = range_text,
                  strs      = str_ct_text,
                  strs_flat = str_flat_text,
                  impacts   = impact_text)
  })
  
}