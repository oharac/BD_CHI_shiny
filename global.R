# global.R

message('running global.R')

## Load packages
library(shinydashboard)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(tippy)
library(here)
library(rgdal)
library(raster)
library(tidyverse)
library(threejs)

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

message('taxa/stressor name mappings for UI')
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

### Read in data
r_nspp <- raster::raster(here::here('data/n_spp_map_latlong.tif'))
nspp_df <- raster::rasterToPoints(r_nspp) %>%
  as.data.frame() %>%
  setNames(c('x', 'y', 'nspp')) %>%
  mutate(cell_id = 1:n()) 

### for impact rasters, let's do as a list to avoid need for filtering by year.
### Find out if Fitz server has multiple cores!
impact_fs <- list.files(here::here('data/impact_maps'), pattern = 'impact_all_[0-9]{4}_latlong.tif',
                        full.names = TRUE)

message('creating impact map year list')
map_year_list <- parallel::mclapply(impact_fs, mc.cores = 4,
                                    FUN = function(f) { ### f <- impact_fs[1]
                                      r <- raster::raster(f) 
                                      r_df <- r %>%
                                        raster::rasterToPoints() %>%
                                        as.data.frame() %>%
                                        setNames(c('x', 'y', 'n_imp')) %>%
                                        mutate(cell_id = 1:n()) %>%
                                        dt_join(nspp_df %>% select(-x, -y), 
                                                by = 'cell_id', type = 'left') %>%
                                        mutate(pct_imp = ifelse(nspp > 0, round(n_imp / nspp * 100), 0))
                                      return(r_df)
                                    }) %>%
  setNames(str_extract(impact_fs, '[0-9]{4}'))

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
