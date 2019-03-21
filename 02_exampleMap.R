
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, broom, sf, tmap)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
shp <- shapefile('../_data/_shp/stb_prc_alta_dpto.shp')
mps <- shapefile('../_data/_shp/stb_prc_alta.shp')

# Download data for the other countries
pnm <- raster::getData('GADM', country = 'PAN', level = 0)
ecu <- raster::getData('GADM', country = 'ECU', level = 0)
ven <- raster::getData('GADM', country = 'VEN', level = 0)
per <- raster::getData('GADM', country = 'PER', level = 0)
bra <- raster::getData('GADM', country = 'BRA', level = 0)

# To make the graph
tdy <- broom::tidy(shp, 'NOMBRE_DPT')
tdy <- as_data_frame(tdy)
tdy <- right_join(tdy, shp@data, by = c('id' = 'NOMBRE_DPT'))

unique(tdy$id)
tdy <- tdy %>% filter(!id %in% 'ARCHIPIÃ‰LAGO DE SAN ANDRÃ‰S, PROVIDENCIA Y SANTA CATALINA')

pnm <- broom::tidy(pnm, 'NAME_0')
ecu <- broom::tidy(ecu, 'NAME_0')
ven <- broom::tidy(ven, 'NAME_0')
per <- broom::tidy(per, 'NAME_0')
bra <- broom::tidy(bra, 'NAME_0')


# To make the map
gg <- ggplot() +
  geom_polygon(data = tdy, aes(x = long, y = lat, group = id, fill = MEAN_prc), colour = 'grey') +
  scale_fill_distiller(palette = 'Spectral') +
  geom_polygon(data = pnm, aes(x = long, y = lat, group = group), fill = NA, colour = "grey") +
  geom_polygon(data = ecu, aes(x = long, y = lat, group = group), fill = NA, colour = "grey") +
  geom_polygon(data = ven, aes(x = long, y = lat, group = group), fill = NA, colour = "grey") +
  geom_polygon(data = per, aes(x = long, y = lat, group = group), fill = NA, colour = "grey") +
  geom_polygon(data = bra, aes(x = long, y = lat, group = group), fill = NA, colour = "grey") +
  coord_equal(xlim = c(-80, -66), ylim = c(-4.22, 13)) +
  xlab('Longitud') +
  ylab('Latitud') +
  labs(fill = 'Idoneidad \nPromedio (%)')
  
ggsave(plot = gg, filename = '../_maps/aptitudPromedioDpt.png', units = 'in', width = 9, height = 12, dpi = 300)

# End




