### Load packages
# library(gganimate)




### Build species bar chart
buildTaxonPlot <- function(impact_df, taxa_input) {
  ### taxa_input <- 'bony fishes'
  
  ### Build bar plot

  
  return(tp)
}

### Build stressor bar chart
buildStressorsOutput <- function(habitat_data, stressor_input) {

    next_stressor_field <- stressor_fields[match(stressor_input, stressor_names)] 
  
  # Get data subset containing previous and current stressor selections
  subset <- habitat_data %>% 
  filter(impact %in% c(current_stressor_field, next_stressor_field)) %>%
  select(impact, habitat, mean) %>% 
  group_by(impact) %>% 
  arrange(impact, -mean) %>% 
  mutate(rank = 1:n(), sqn = case_when(impact == current_stressor_field ~ 1, TRUE ~ 2))
  
  # Update current stressor global
  current_stressor <<- stressor_input
  
  # Build ranked bar plot
  tp <- subset %>%  
  ggplot() +  
  aes(xmin = 0,  
    xmax = mean) +  
  aes(ymin = rank - .45,  
    ymax = rank + .45,  
    y = rank) +  
  facet_wrap(~ impact) +  
  geom_rect(alpha = .7) +  
  aes(fill = habitat) +
  scale_x_continuous(
    limits = c(-0.2, 0.9),
    breaks = c(0, .225, .45, 0.675)) +
  geom_text(col = 'white',  
    hjust = 'right',  
    aes(label = habitat),  
    x = -0.025) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Impact Metric') +  
  labs(y = '') +
  plot_theme
  
  # Build plot with transitions between previous and current value
  tpp <- tp + facet_null() + 
  aes(group = habitat) +
  transition_states(
    sqn,
    transition_length = 1,
    state_length = 1,
    wrap = FALSE
    )+
  ease_aes('linear')
  
  # Temp save of animated gif
  anim <- animate(tpp, fps = 20, nframes = 80, renderer = gifski_renderer(loop = F))
  anim_save('outfile_stressor.gif', anim)
  
  # Return the gif file
  list(src = 'outfile_stressor.gif',
   contentType = 'image/gif'
   )
}

## Build the caption for habitats plot
buildTaxonCaption <- function(tx) {
  sprintf('Mean impacted range on %s by stressor', tx)
}

## Build the caption for stressors plot
buildStressorCaption <- function(stressor) {
  paste('Mean impact by taxon for %s', stressor)
}

