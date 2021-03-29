### setup script: read in data, create helper functions

### helper fxns
dt_join <- function (df1, df2, by, type) {
  a <- case_when(type == "left"  ~ c(FALSE, TRUE, FALSE), 
                 type == "full"  ~ c(TRUE, TRUE, TRUE), 
                 type == "inner" ~ c(FALSE, FALSE, FALSE))
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2, all = a[1], all.x = a[2], all.y = a[3])
  return(as.data.frame(dt_full))
}


### Read in data
r_nspp <- raster::raster(here('data/n_spp_map_latlong.tif'))
nspp_df <- rasterToPoints(r_nspp) %>%
  as.data.frame() %>%
  setNames(c('x', 'y', 'nspp')) %>%
  mutate(cell_id = 1:n()) 

### for impact rasters, let's do as a list to avoid need for filtering by year.
### Find out if Fitz server has multiple cores!
impact_fs <- list.files(here('data/impact_maps'), pattern = 'impact_all_[0-9]{4}_latlong.tif',
                        full.names = TRUE)

impact_list <- parallel::mclapply(impact_fs, mc.cores = 4,
                 FUN = function(f) { ### f <- impact_fs[1]
                   r <- raster::raster(f) 
                   r_df <- r %>%
                     rasterToPoints() %>%
                     as.data.frame() %>%
                     setNames(c('x', 'y', 'n_imp')) %>%
                     mutate(cell_id = 1:n()) %>%
                     dt_join(nspp_df %>% select(-x, -y), 
                             by = 'cell_id', type = 'left') %>%
                     mutate(pct_imp = ifelse(nspp > 0, round(n_imp / nspp * 100), 0))
                   return(r_df)
                 }) %>%
  setNames(str_extract(impact_fs, '[0-9]{4}'))

global_trends <- read_csv(here("data", "global_impact_trends.csv"))
habitat_data <- read_csv(here("data", "habitat_impacts.csv"))


### Set initial global values
current_habitat <<- "coral_reef"
current_stressor <<- "shipping"
globePlot <<- NULL


