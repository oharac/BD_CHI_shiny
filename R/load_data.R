### Read in data
r_nspp <- raster::raster(here('data/n_spp_map_latlong.tif'))
nspp_df <- raster::rasterToPoints(r_nspp) %>%
  as.data.frame() %>%
  setNames(c('x', 'y', 'nspp')) %>%
  mutate(cell_id = 1:n()) 

### for impact rasters, let's do as a list to avoid need for filtering by year.
### Find out if Fitz server has multiple cores!
impact_fs <- list.files(here('data/impact_maps'), pattern = 'impact_all_[0-9]{4}_latlong.tif',
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
spp_impact_data <- read_csv(here('data', 'imp_range_by_spp_2013.csv')) %>%
  distinct()
spp_risk <- read_csv(here('data', 'iucn_risk_2020-1.csv'))
spp_taxa <- read_csv(here('data', 'iucn_taxa_2020-1.csv'))

impact_df <- spp_impact_data %>%
  dt_join(spp_risk, type = 'left', by = 'iucn_sid') %>%
  dt_join(spp_taxa %>% select(-sciname), type = 'left', by = c('iucn_sid')) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
  mutate(impact_pct = impact_km2 / range_km2,
         impact_pct = ifelse(is.nan(impact_pct), 0, impact_pct)) %>%
  filter(stressor != 'cum_ocean')
