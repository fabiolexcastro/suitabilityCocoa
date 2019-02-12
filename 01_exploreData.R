
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, velox, sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
lbl <- data.frame(vls = c(0, 1, 2, 3, 8), ctg = c('No Apto', 'Aptitud baja', 'Aptitud media', 'Aptitud alta', 'Exclusion legal'))

# Load data
stb <- raster('../_data/_stb/suitCocoa_geo.tif')
adm <- shapefile('../_data/_shp/Municipios_SIGOT_geo.shp')
sft <- st_as_sf(shp)

# Aggregate
str(adm@data)
adm@data$ID_ESPACIA <- as.numeric(adm@data$ID_ESPACIA)
ad2 <- aggregate(x = adm, by = 'ID_ESPACIA')
ad2lyr <- rasterize(ad2, stb, field = 'ID_ESPACIA')

# Extract the suitabiliy type by municipality
pnt <- rasterToPoints(stb, spatial = TRUE)
vlx <- velox(ad2lyr)
mps <- vlx$extract_points(sp = pnt)
pnt@data$gid <- mps %>% as.vector()
colnames(pnt@data) <- c('stb', 'gid')

# Write the final shapefile
writeOGR(obj = pnt, dsn = '../_data/_shp', layer = 'stb_pnt_adm', driver = 'ESRI Shapefile')

# TIdy the shapefile
dfm <- as_data_frame(pnt)
dfm <- inner_join(dfm, adm@data %>% dplyr::select(ID_ESPACIA, NOM_MUNICI, NOMBRE_DPT, area_ha), by = c('gid' = 'ID_ESPACIA'))
dfm <- dfm %>% mutate(NOM_MUNICI = iconv(NOM_MUNICI, 'UTF-8', 'latin1'), NOMBRE_DPT = iconv(NOMBRE_DPT, 'UTF-8', 'latin1'))

# Group by suitability
lbl <- data.frame(vls = c(0, 1, 2, 3, 8), ctg = c('Sin idoneidad', 'Baja', 'Media', 'Alta', 'Exclusion legal'))
dfm <- inner_join(dfm, lbl, by = c('stb' = 'vls'))
smm <- dfm %>%
  mutate(n = rep(x = 1, times = nrow(dfm))) %>% 
  group_by(ctg, gid, NOMBRE_DPT, NOM_MUNICI) %>%
  summarise(sum_n = sum(n)) %>%
  ungroup()

# Group by mask
pix <- stb * 0 + 1
pix <- rasterToPoints(pix, spatial = TRUE)
pix <- vlx$extract_points(sp = pix)
pix <- pix %>% as_data_frame %>% mutate(n = rep(x = 1, times = nrow(.)))
pix <- pix %>% 
  setNames(c('gid', 'n')) %>% 
  group_by(gid) %>%
  summarise(sum_n = sum(n)) %>% 
  ungroup() %>%
  setNames(c('gid', 'sum_total'))

# Join both tables into only one
area <- smm %>%
  group_by(gid) %>%
  summarize(sum = sum(sum_n)) %>%
  ungroup() %>%
  inner_join(., pix, by = 'gid')

fnl <- inner_join(smm, pix, by = 'gid') %>%
  mutate(prc = sum_n / sum_total * 100) 
fnl <- inner_join(st_as_sf(adm) %>% dplyr::select(ID_ESPACIA), fnl, by = c('ID_ESPACIA' = 'gid')) 
fnl <- fnl %>% mutate(prc = round(prc, 2))
st_write(obj = fnl, dsn = '../_data/_shp', layer = 'stb_prc', driver = 'ESRI Shapefile', update = TRUE)

# Reviewing the duplicated files
adm_df <- adm %>% as_data_frame() %>% dplyr::select(ID_ESPACIA, NOM_MUNICI, NOMBRE_DPT) %>% mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, 'UTF-8', 'latin1'), NOM_MUNICI = iconv(NOM_MUNICI, 'UTF-8', 'latin1'))
adm_dp <- adm_df[duplicated(adm_df$ID_ESPACIA),]
adm_dp <- adm_df %>% filter(ID_ESPACIA %in% adm_dp$ID_ESPACIA)
write.csv(adm_dp, '../_data/_tbl/dup_adm.csv', row.names = F)



