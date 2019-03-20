
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, broom, sf, tmap)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
# shp <- shapefile('../_data/_shp/stb_prc_alta_dpto.shp')
mps <- shapefile('../_data/_shp/stb_prc_alta.shp')

# To make the graph
spplot(mps, 'prc')


# tdy <- broom::tidy(shp, region = 'MEAN_prc')
# tdy <- tdy %>% mutate(group = as.numeric(group))
# frt <- fortify(shp, 'MEAN_prc')
# 
# sft <- st_as_sf(shp)
# sft <- sft %>% rename(Idoneidad_Porcentaje = MEAN_prc)
# 
# tm_shape(sft) +
#   tm_polygons('Idoneidad_Porcentaje', palette = 'Greens')
# tm_shape(mps) +
#   tm_polygons('prc', palette = 'Greens')

# ggplot(sft) +
#   geom_sf(aes(fill = MEAN_prc)) +
#   scale_fill_gradientn(low = '#red', high = 'green')
# 
# 
# gg <- ggplot() +
#   geom_polygon(data = shp, aes(long, lat, group = group, fill = MEAN_prc)) +
#   xlab('Longitud') +
#   ylab('Latitud') +
#   scale_fill_continuous(low = 'red', high = 'green') +
#   coord_equal()
# 
# 
# ggsave(plot = gg, filename = 'suit_alto_prc.png', units = 'cm', height = 15, width = 9, dpi = 300)







