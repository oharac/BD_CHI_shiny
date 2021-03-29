
message('is it running THIS TOO?')
# # Experimental data prep
# 
# library(rgdal)
# library(tiff)
# library(threejs)
# library(raster)
# library(here)
# library(tidyverse)
# library(plotly)
# library(rworldmap)
# 
# # Target CRS for reprojection
# target <-  crs("+proj=longlat +datum=WGS84 +no_defs")
# 
# my_files <- list.files(path = here("CHI", "annual"), full.names = TRUE)
# raster_stack <- raster::stack(my_files)
# 
# raster_agg <- aggregate(raster_stack, fac = 100)
# 
# reprojected <- projectRaster(raster_agg, crs=target)
# 
# raster_df <- rasterToPoints(reprojected) %>% 
# as.data.frame()
# 
# names(raster_df)[1] <- "long"
# names(raster_df)[2] <- "lat"
# 
# # Cutoff overlap
# raster_df <- raster_df %>%
# filter(between(long, -180, 180))
# 
# # globe
# globejs(lat=raster_df$lat,
#   long=raster_df$long,
#   val=raster_df["cumulative_impact_2013"],
#   pointsize=1,
#   atmosphere=TRUE)
# 
# 
# write.csv(raster_df, here("data", "cumulative_impact_2013.csv"), row.names = FALSE)
# 
# 
# # -----
# 
# annual_impact <- read_csv(here("data", "cumulative_impacts_annual.csv"))
# selected_year <- paste("cumulative_impact_", "2013", sep = "") 
# 
# # Quartile of each value
# annual_impact$q <- as.numeric(
#   cut(annual_impact[[selected_year]],
#     breaks=quantile(annual_impact[[selected_year]], probs=c(0,0.25,0.5,0.75,1)),
#     include.lowest=TRUE))
# # Colors for each level
# col = c("#0055ff","#00aaff","#00ffaa","#aaff00")[annual_impact$q]
# 
# globejs(lat=annual_impact$lat,
#   long=annual_impact$long,
#   val=annual_impact[[selected_year]] * 20,
#   pointsize=1,
#   color = col,
#   atmosphere=TRUE)
# 
# #---
# 
# library(tidyverse)
# library(here)
# library(gganimate)
# library(tweenr)
# 
# habitat_data <- read_csv(here("data", "habitat_impacts.csv"))
# 
# 
# 
# current_data <- habitat_data %>% 
# filter(impact == current_habitat) %>%
# select(impact, habitat, mean)
# 
# next_data <- habitat_data %>% 
# filter(impact == next_habitat) %>%
# select(impact, habitat, mean)
# 
# tween_data <- tween_states(list(current_data, next_data), .05, 0, 'linear', 40) %>% 
# group_by(.frame) %>% 
# arrange(.frame, mean) %>% 
# mutate(rank = 1:n())
# 
# 
# tp <- tween_data %>% 
# ggplot() +
# aes(x= mean, y = rank) +
# facet_wrap(~ impact) + geom_col()
# #---
# current_habitat <- "salt_marsh"
# next_habitat <- "rocky_reef"
# 
# 
# my_theme <- theme_classic() +
# theme(axis.text.y = element_blank()) +
# theme(axis.ticks.y = element_blank()) +
# theme(axis.line.y = element_blank()) +
# theme(legend.position = "none") +
# theme(plot.background = element_rect(fill = "black")) +
# theme(panel.background = element_rect(fill = "black"))
# 
# 
# subset <- habitat_data %>% 
# filter(habitat %in% c(current_habitat, next_habitat)) %>%
# select(impact, habitat, mean) %>% 
# group_by(habitat) %>% 
# arrange(habitat, -mean) %>% 
# mutate(rank = 1:n())
# 
# tp <- subset %>%  
# ggplot() +  
# aes(xmin = 0,  
#   xmax = mean) +  
# aes(ymin = rank - .45,  
#   ymax = rank + .45,  
#   y = rank) +  
# facet_wrap(~ habitat) +  
# geom_rect(alpha = .7) +  
# aes(fill = impact) +
# scale_x_continuous(
#   limits = c(-0.2, 0.9),
#   breaks = c(0, .225, .45, 0.675)) +
# geom_text(col = "gray13",  
#   hjust = "right",  
#   aes(label = impact),  
#   x = -0.025) +  
# scale_y_reverse() +  
# labs(fill = NULL) +  
# labs(x = 'Impact Metric') +  
# labs(y = "") +
# my_theme
# 
# tpp <- tp + facet_null() + 
# aes(group = impact) +
# transition_states(
#   habitat,
#   transition_length = 1,
#   state_length = 0,
#   wrap = FALSE
#   )+
# ease_aes('linear')
# anim <- animate(tpp, fps = 20, nframes = 80, renderer = gifski_renderer(loop = F))
# anim_save("outfile.gif", anim)
# #---
# 
# 
# p1 <- tween_data %>% 
# ggplot(aes(x = habitat, y = mean, frame = .frame)) +
# geom_bar(stat = "identity") +
# coord_flip() +
# transition_states(.frame)
# animate(p1, nframes = 40, duration = 2, renderer = gifski_renderer(loop = F))
# 
# 
# 
# subset <- habitat_data %>% 
# filter(impact %in% c(current_habitat, next_habitat)) %>%
# select(impact, habitat, mean) %>% 
# arrange(habitat)
# 
# 
# p2 <- ggplot(subset, aes(habitat, y = mean)) +
# geom_col() +
# coord_flip() +
# transition_states(
#   impact,
#   transition_length = 1,
#   state_length = 0,
#   wrap = FALSE
#   ) +
# ease_aes('linear')
# animate(p2, fps = 20, nframes = 40, renderer = gifski_renderer(loop = F))
# 
# 
# # --
# x <- raster::raster(here("CHI", "cumulative_impact_trend_2003-2013.tif"))
# 
# x2 <- aggregate(x, fac = 200)
# 
# target <-  crs("+proj=longlat +datum=WGS84 +no_defs")
# 
# xog <- projectRaster(x2, crs=crs(x))
# xogdf <- rasterToPoints(xog) %>% 
# as.data.frame()
# 
# names(xogdf) <- c("long","lat","value")
# 
# xogdf_trimmed <- xogdf %>%
# filter(between(lat, -85, 90))
# 
# ggplotly(ggplot(data = xogdf_trimmed, aes(x=long, y = lat, color = value)) +
#   geom_point() + theme_void())
# 
# 
# x2 <- projectRaster(x2, crs=target)
# 
# trend_df <- rasterToPoints(x2) %>% 
# as.data.frame()
# 
# names(trend_df) <- c("long","lat","value")
# 
# trend_df <- trend_df %>%
# filter(between(long, -180, 180)) %>% 
# mutate(value = value * 100)
# 
# trend_df_trimmed <- trend_df %>%
# filter(between(lat, -84, 90))
# 
# # interval breaks for color buckets
# trend_df$q <- as.numeric(
#   cut(trend_df$value,
#     breaks=quantile(trend_df$value, probs=c(0,0.25,0.5,0.75,1)),
#     include.lowest=TRUE))
# 
# # Colors for each level
# col = c("#0055ff","#00aaff","#00ffaa","#aaff00")[trend_df$q]
# 
# # bling out the data
# globejs(lat=trend_df$lat, long=trend_df$long,
#   val=trend_df$value*2,
#   color = col,
#   pointsize=1,
#   atmosphere=TRUE)
# 
# countries_sf <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium")
# 
# ggplotly(ggplot(data = trend_df_trimmed, aes(x=long, y = lat, color = value)) +
#   geom_point() +
#   theme_void() +
#   theme(
#     panel.background = element_rect(fill = "black"),
#     plot.background = element_rect(fill = "black")
#     ) +
#   borders("world", fill="black"))
# 
# sPDF <- getMap()  
# #mapCountries using the 'continent' attribute  
# mapCountryData(sPDF, nameColumnToPlot='continent')
# 
# 
# ggplot(data = trend_df_trimmed, aes(x=long, y = lat, color = value)) +
# geom_point() + theme_void() + borders("world", fill="white")
# 
# crs(x2)
# 
# write.csv(trend_df_trimmed, here("data", "global_impact_trends.csv"), row.names = FALSE)
