# global.R

message('running global.R')

## Load packages
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(tippy)
library(here)
library(rgdal)
library(raster)
library(tidyverse)
library(threejs)

# install.packages(c('shinydashboard','shiny','shinyBS','shinyjs','shinycssloaders','shinyWidgets','tippy','here','rgdal','raster','tidyverse','threejs'))

### dt_join is datatable version of dplyr joins, for speed
dt_join <- function (df1, df2, by, type) {
  a <- case_when(type == 'left'  ~ c(FALSE, TRUE, FALSE), 
                 type == 'full'  ~ c(TRUE, TRUE, TRUE), 
                 type == 'inner' ~ c(FALSE, FALSE, FALSE))
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2, all = a[1], all.x = a[2], all.y = a[3])
  return(as.data.frame(dt_full))
}

### define a function for transforming species richness
richness_xfm <- function(x) {
  x^.7 ### a little less drastic than sqrt
}

taxa_names <- read_csv(here('data/iucn_taxa_2020-1.csv')) %>%
  select(tx_field = assess_gp, tx_name = desc) %>%
  distinct() %>%
  bind_rows(data.frame(tx_field = 'all', tx_name = 'all at-risk species')) %>%
  arrange(tx_name) %>%
  mutate(tx_field = fct_inorder(tx_field),
         tx_name = fct_inorder(tx_name))

str_names <- read_csv(here('data/stressor_names.csv')) %>%
  distinct() %>%
  mutate(str_field = fct_inorder(str_field),
         str_name = fct_inorder(str_name),
         category = fct_inorder(category))

str_table <- str_names %>%
  filter(!str_detect(str_field, 'cum_')) %>%
  mutate(str_name = str_remove(str_name, ' \\(.+\\)'),
         category = str_to_title(category)) %>%
  select(Category = category, `Stressor name` = str_name)

######################################################
### generate list of raster/dfs for impact by year ###
######################################################
message('generating list of raster dfs for impact')
r_nspp <- raster::raster(here::here('data/n_spp_map_latlong.tif'))
nspp_df <- raster::rasterToPoints(r_nspp) %>%
  as.data.frame() %>%
  setNames(c('x', 'y', 'nspp'))

### let's do as a list to avoid need for filtering by year.
### Find out if Fitz server has multiple cores!
# impact_fs <- list.files(here::here('data/impact_maps'), pattern = 'impact_all_[0-9]{4}_latlong.tif',
#                         full.names = TRUE)
impact_fs <- list.files(here::here('data/impact_maps'), pattern = 'impact_.+_20.3_latlong.tif',
                        full.names = TRUE)

message('creating impact map year list')
impact_map_list <- lapply(impact_fs, 
                          FUN = function(f) { ### f <- impact_fs[1]
                            r <- raster::raster(f) 
                            r_df <- r %>%
                              raster::rasterToPoints() %>%
                              as.data.frame() %>%
                              setNames(c('x', 'y', 'n_imp')) %>%
                              dt_join(nspp_df, 
                                      by = c('x', 'y'), type = 'left') %>%
                              mutate(pct_imp = ifelse(nspp > 0, round(n_imp / nspp * 100), 0))
                            return(r_df)
                          }) %>%
  setNames(str_remove_all(basename(impact_fs), 'impact_|_latlong.tif'))

#########################################
###      set up difference list       ###
#########################################
message('creating difference list')
strs_vec <- c('all', 'fishing', 'climate', 'land-based', 'ocean')
impact_diff_list <- lapply(strs_vec,
                           FUN = function(s) { ### s <- strs_vec[1]
                             df03 <- impact_map_list[[paste(s, 2003, sep = '_')]] %>%
                               select(x, y, pct_03 = pct_imp)
                             df13 <- impact_map_list[[paste(s, 2013, sep = '_')]] %>%
                               select(x, y, nspp, pct_13 = pct_imp)
                             diff <- df13 %>% left_join(df03) %>%
                               mutate(delta = pct_13 - pct_03)
                             return(diff)
                           }) %>%
  setNames(paste0('diff_', strs_vec))

#########################################
### generate df of impacts by species ###
#########################################
message('reading in impacted range by species')

spp_impact_data <- read_csv(here::here('data', 'imp_range_by_spp_2013.csv')) %>%
  distinct()
spp_risk <- read_csv(here::here('data', 'iucn_risk_2020-1.csv'))
spp_taxa <- read_csv(here::here('data', 'iucn_taxa_2020-1.csv'))

impact_df <- spp_impact_data %>%
  dt_join(spp_risk, type = 'left', by = 'iucn_sid') %>%
  dt_join(spp_taxa %>% select(-sciname), type = 'left', by = c('iucn_sid')) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(impact_pct = impact_km2 / range_km2,
         impact_pct = ifelse(is.nan(impact_pct), 0, impact_pct)) %>%
  filter(stressor != 'cum_ocean')

#######################################################
### generate list of raster/dfs for intensification ###
#######################################################
message('generating list of raster dfs for intensification')

intens_fs <- list.files(here::here('data/intens_maps'), 
                        pattern = 'intens_.+_latlong.tif',
                        full.names = TRUE)

intens_r_list <- lapply(intens_fs, 
                        FUN = function(f) { ### f <- intens_fs[1]
                          r <- raster::raster(f)
                          r_df <- r %>%
                            raster::rasterToPoints() %>%
                            as.data.frame() %>%
                            setNames(c('x', 'y', 'n_int')) %>%
                            dt_join(nspp_df, 
                                    by = c('x', 'y'), type = 'left') %>%
                            mutate(pct_int = ifelse(nspp > 0, round(n_int / nspp * 100), 0))
                          return(r_df)
                        }) %>%
  setNames(str_remove_all(intens_fs, '.+intens_|2.+'))

message('creating calc_spp_df dataframe')
calc_spp_df <- impact_df %>%
  filter(iucn_sid %in% c(8005, 19488, 39374, 2478, 7750, 21860, 132928, 22694870)) %>%
  select(iucn_sid, sciname, comname, desc) %>% 
  distinct() %>%
  left_join(spp_impact_data) %>%
  left_join(str_names, by = c('stressor' = 'str_field')) %>%
  filter(!is.na(impact_km2)) %>%
  filter(!str_detect(stressor, 'cum_')) %>%
  group_by(iucn_sid, sciname, comname, desc) %>%
  summarize(strs = paste(str_name, collapse = ', '))
  
message('calc_spp_df has ', nrow(calc_spp_df), ' rows')

