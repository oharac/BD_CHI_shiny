# library(raster)
# library(tidyverse)
# library(here)
# 
# ### transform impact rasters to 4326
# res_out <- 1 ### in degrees
# 
# r_fs <- list.files(here('data/impact_maps'), pattern = 'impact_all_[0-9]{4}.tif', 
#                    full.names = TRUE)
# r_out_fs <- str_replace(r_fs, '.tif', '_latlong.tif')
# base_rast <- raster(ext = extent(c(-180, +180, -90, +90)), 
#                     res = res_out, crs = '+init=epsg:4326')
# 
# reload <- TRUE
# if(any(!file.exists(r_out_fs)) | reload) {
#   r_impacts <- stack(r_fs)
# 
#   r_latlong <- projectRaster(r_impacts, base_rast, 
#                              method = 'ngb',
#                              progress = 'text')
#   
#   r_out_fs <- str_replace(r_fs, '.tif', '_latlong.tif')
#   writeRaster(r_latlong, bylayer = TRUE, filename = r_out_fs, overwrite = TRUE)
# }
# 
# ### transform species richness raster to 4326
# r_nspp <- raster(here('data/n_spp_map.tif'))
# r_nspp_latlong <- projectRaster(r_nspp, base_rast, 
#                                 method = 'ngb',
#                                 progress = 'text')
# writeRaster(r_nspp_latlong, filename = here('data/n_spp_map_latlong.tif'), overwrite = TRUE)
# 
#          