
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, sf, tidyverse, velox, sf)

# Functions to use 
mySummary <- function(ctg){
  # ctg <- 'Bajo-Bajo'
  sub <- sft %>% 
    as.data.frame() %>% 
    filter(category == ctg) %>% 
    dplyr::select(ID_ESPACIA, category) %>% 
    as_data_frame() 
  rsl <- inner_join(sub, smm_cacao, by = c('ID_ESPACIA' = 'COD_MUNI'))
  print('Done!')
  return(rsl)
}

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

lbl <- data.frame(value = 0:5, category = c('Sin significancia', 'Alto-Alto', 'Bajo-Bajo', 'Bajo-Alto', 'Alto-Bajo', 'Sin vecinos'))
shp <- st_read('../_data/_shp/final_shape.shp')

sft <- inner_join(shp, lbl, by = c('LISA_CL' = 'value'))

smm <- sft %>% 
  as.data.frame() %>% 
  group_by(category) %>% 
  summarize(count = n()) %>%
  ungroup()

# Diferentes combinaiones de los clusters - conteo de municipios y de los dptos con tales municipios
sft %>% 
  as.data.frame() %>% 
  filter(category == 'Bajo-Bajo') %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, category) %>% 
  mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, from = 'UTF-8', to = 'latin1')) %>% 
  group_by(NOMBRE_DPT) %>% 
  summarize(count = n()) %>% 
  ungroup()

sft %>% 
  as.data.frame() %>% 
  filter(category == 'Alto-Alto') %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, category) %>% 
  mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, from = 'UTF-8', to = 'latin1')) %>% 
  group_by(NOMBRE_DPT) %>% 
  summarize(count = n()) %>% 
  ungroup()

sft %>% 
  as.data.frame() %>% 
  filter(category == 'Bajo-Alto') %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, category) %>% 
  mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, from = 'UTF-8', to = 'latin1')) %>% 
  group_by(NOMBRE_DPT) %>% 
  summarize(count = n()) %>% 
  ungroup()

sft %>% 
  as.data.frame() %>% 
  filter(category == 'Alto-Bajo') %>% 
  dplyr::select(NOMBRE_DPT, NOM_MUNICI, category) %>% 
  mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, from = 'UTF-8', to = 'latin1')) %>% 
  group_by(NOMBRE_DPT) %>% 
  summarize(count = n()) %>% 
  ungroup()

# Unión con datos de producción -------------------------------------------
cacao <- read_csv('D:/_academico/_maestria/_tesis/_data/_tbl/_prd/production_tidy_all.csv')
yrs_cacao <- unique(cacao$PERIODO)
cacao <- cacao %>% filter(PERIODO %in% 2007:2017)
smm_cacao <- cacao %>% 
  group_by(COD_MUNI, DPTO, MPIO) %>%
  summarize(crp = mean(AREA_SEMBRADA_HA, na.rm = TRUE),
            hrv = mean(AREA_COSECHADA_HA, na.rm = TRUE),
            prd = mean(PRODUCCION, na.rm = TRUE),
            rdt = mean(RDTOS, na.rm = TRUE)) %>%
  ungroup()
smm_cacao <- smm_cacao %>% mutate(prc_crp = crp / sum(crp) * 100)

# Apply the function
low.low <- mySummary(ctg = 'Bajo-Bajo') %>% mutate(DPTO = iconv(DPTO, to = 'latin1'), MPIO = iconv(MPIO, to = 'latin1'))




