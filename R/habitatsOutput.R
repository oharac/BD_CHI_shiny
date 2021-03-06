## Load packages
library(tidyverse)
library(gganimate)

## Habitat/stressor name mappings
habitat_names <- c("beach","coral reef","deep hard bottom","deep seamount","deep soft benthic","deep water","hard shelf (60-200m)","hard slope (200-2000m)",
 "intertidal mud","kelp forest","mangrove","rocky intertidal","rocky reef","salt marsh","seagrass","soft shelf (60-200m)",
 "soft slope (200-2000m)","subtidal soft bottom","surface water","susp. feeder reef")
habitat_fields <- c("beach","coral_reef","d_h_bottom","seamounts","d_s_benthic","deep_waters","hard_shelf","hard_slope",
  "inttidalmud","kelp","mangroves","rky_intidal","rocky_reef","salt_marsh","seagrass","soft_shelf",
  "soft_slope","s_t_s_bottom","surface_waters","suspension_reef")

stressor_fields <- c("art_fish","dem_dest","dem_nondest_hb","dem_nondest_lb","direct_human","light",
 "nutrient","oa","organic","pel_hb","pel_lb","shipping","slr","sst")
stressor_names <- c("artisanal fishing","comm fish: dem dest","comm fish: dem nondest hb","comm fish: dem nondest lb","direct human","light pollution",        
  "nutrient pollution","oa","organic pollution","comm fish: pel hb","comm fish: pel lb","shipping","slr","sst" )

## ggplot theme
plot_theme <- theme_classic() +
theme(axis.text.y = element_blank()) +
theme(axis.ticks.y = element_blank()) +
theme(axis.line.y = element_blank()) +
theme(legend.position = "none") +
theme(plot.background = element_rect(fill = "black")) +
theme(panel.background = element_rect(fill = "black")) +
theme(axis.text.x = element_text(color = "white"))

## Build habitat animated bar chart
buildHabitatsOutput <- function(habitat_data, habitat_input, current_habitat){
  current_habitat_field <- habitat_fields[match(current_habitat, habitat_names)]
  next_habitat_field <- habitat_fields[match(habitat_input, habitat_names)] 
  
  # Get data subset containing previous and current habitat selections
  subset <- habitat_data %>% 
  filter(habitat %in% c(current_habitat_field, next_habitat_field)) %>%
  select(impact, habitat, mean) %>% 
  group_by(habitat) %>% 
  arrange(habitat, -mean) %>% 
  mutate(rank = 1:n(), sqn = case_when(habitat == current_habitat_field ~ 1, TRUE ~ 2))
  
  # Update current habitat global
  current_habitat <<- habitat_input
  
  # Build ranked bar plot
  tp <- subset %>%  
  ggplot() +  
  aes(xmin = 0,  
    xmax = mean) +  
  aes(ymin = rank - .45,  
    ymax = rank + .45,  
    y = rank) +  
  facet_wrap(~ habitat) +  
  geom_rect(alpha = .7) +  
  aes(fill = impact) +
  scale_x_continuous(
    limits = c(-0.2, 0.9),
    breaks = c(0, .225, .45, 0.675)) +
  geom_text(col = "white",  
    hjust = "right",  
    aes(label = impact),  
    x = -0.025) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Impact Metric') +  
  labs(y = "") +
  plot_theme
  
  # Build plot with transitions between previous and current value
  tpp <- tp + facet_null() + 
  aes(group = impact) +
  transition_states(
    sqn,
    transition_length = 1,
    state_length = 1,
    wrap = FALSE
    )+
  ease_aes('linear')
  
  # Temp save of animated gif
  anim <- animate(tpp, fps = 20, nframes = 80, renderer = gifski_renderer(loop = F))
  anim_save("outfile_habitat.gif", anim)
  
  # Return the gif file
  list(src = "outfile_habitat.gif",
   contentType = 'image/gif'
   )
}

## Build stressor animated bar chart
buildStressorsOutput <- function(habitat_data, stressor_input, current_stressor) {
  current_stressor_field <- stressor_fields[match(current_stressor, stressor_names)]
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
  geom_text(col = "white",  
    hjust = "right",  
    aes(label = habitat),  
    x = -0.025) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'Impact Metric') +  
  labs(y = "") +
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
  anim_save("outfile_stressor.gif", anim)
  
  # Return the gif file
  list(src = "outfile_stressor.gif",
   contentType = 'image/gif'
   )
}

## Build the caption for habitats plot
buildHabitatCaption <- function(habitat) {
  paste("Breakdown of ", habitat, " impact by individual stressors.", sep = "")
}

## Build the caption for stressors plot
buildStressorCaption <- function(stressor) {
  paste("Habitats being impacted by the ", stressor, " stressor.", sep = "")
}

