# Packages ----
library(tidyverse)
library(lubridate)
library(sf)
library(raster)
library(MODIStsp)
library(MODISTools) #Permite trabajar con datos procesados en vez de bandas espectrales
library(rworldxtra)

# Instalación rgee ----
remotes::install_github("r-spatial/rgee")

library(rgee)

ee_install()

ee_Initialize()

# MODISTools ----
# ¿Qué producto MODIS ocupar?
  # mt_products(): Permite observar la variedad de productos MODIS a los que podemos acceder
products <- mt_products()

MODIS::getTile()

# ¿Qué bandas podemos ocupar?
  # mt_bands(): Revisar las bandas del producto seleccionado que podemos descargar
bands <- mt_bands(product = "MOD13Q1")

# ¿En qué fechas?
  # mt_dates(): Revisar las fechas de la información disponible
dates <- mt_dates(product = "MOD13Q1",
                  lat = -54,
                  lon = -68) %>% 
  mutate(calendar_date = lubridate::ymd(calendar_date)) %>% 
  arrange(desc(calendar_date))

# Descarga de las bandas (en formato data frame)
TDF_NDVI <- MODISTools::mt_subset(product = "MOD13Q1",
                                  lat = -54,
                                  lon = -70,
                                  band = "250m_16_days_NDVI",
                                  start = "2020-01-01",
                                  end = "2020-06-01",
                                  km_lr = 50,
                                  km_ab = 50,
                                  site_name = "Tierra del fuego",
                                  internal = T,
                                  progress = F)

# Conversión a raster
TDF_Raster <- mt_to_raster(df = TDF_NDVI,
                           reproject = T)

# Revisando los valores límites observamos que los valores menores a -0.2 debiesen ser eliminados
values(TDF_Raster) <- ifelse(values(TDF_Raster) < -0.2, NA, values(TDF_Raster))

# Gráfico
rasterVis::levelplot(TDF_Raster)


# Conversión a df
data("countriesHigh")

TDF_SF <- countriesHigh %>% 
  st_as_sf() %>% 
  # Siempre es bueno emplear st_make_valid() cuando trabajamos con sf. Corrige las geometries que no se encuentran adecuadas al cambiar un formato a sf
  st_make_valid() %>% 
  st_crop(TDF_Raster)

TDF_Raster_DF <- TDF_Raster %>% 
  as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% 
  # Convertir los títulos en datos
  pivot_longer(starts_with("X20"),
               names_to = "Fecha",
               values_to = "NDVI") %>% 
  mutate(Fecha = str_remove_all(string = Fecha, pattern = "X"),
         Fecha = ymd(Fecha))

ggplot() + 
  geom_raster(data = TDF_Raster_DF, aes(x = x, y = y, fill = NDVI)) +
  geom_sf(data = TDF_SF, alpha = 0) +
  facet_wrap(~Fecha) + 
  scale_fill_viridis_c()

# ¿Podes mostrar un gráfico x -y del NDVI en el tiempo para un punto en el mapa?
TDF_Raster_DF_punto <- TDF_Raster_DF %>% 
  # filtro para no escoger una observación con NA
  filter(!is.na(NDVI)) %>% 
  # slice(): Te permite seleccionar filas de acuerdo a la posición. IMPORTANTE: Existe una familia de funciones slice() muy interesantes (https://dplyr.tidyverse.org/reference/slice.html)
  slice_sample(n = 1)

ggplot(TDF_Raster_DF_punto, aes(x = Fecha, y = NDVI)) +
  geom_path()

# Landcover ----
# Existe un producto MODIS que integra un LC
bandas <- mt_bands("MCD12Q1")

dates <- mt_dates(product = "MCD12Q1",
                  lat = -54,
                  lon = -68) %>% 
  mutate(calendar_date = lubridate::ymd(calendar_date)) %>% 
  arrange(desc(calendar_date))

TDF_Type <- mt_subset(product = "MCD12Q1", 
                      lat = -54, 
                      lon = -68,
                      band = "LC_Type1",
                      start = as.character(dates$calendar_date[1]),
                      end = "2020-06-02",
                      km_lr = 50,
                      km_ab = 50,
                      site_name = "Tierra del fuego",
                      internal = T,
                      progress = F)

TDF_Raster_Type <- mt_to_raster(df = TDF_Type,
                                reproject = T)













