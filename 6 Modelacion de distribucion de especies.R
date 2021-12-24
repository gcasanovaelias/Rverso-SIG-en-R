# Packages ----
install.packages(c("BIEN", "dismo", "ENMeval", "maxnet"))
library(BIEN) # Base de datos de la Botanical Information and Ecology Network
library(dismo)
library(ENMeval)
library(maxnet)
library(raster)
library(rworldxtra)
library(sf)
library(tidyverse)

# Species Distribution Models (SDM) ----
#* Los SDM (tambien llamado de nichos) modelan la distribución potencial, no real.
#* Se basan en modelos de sólo presencia, no se suelen registrar las ausencias

# Obtengamos presencias de la lenga
N_pumilio <- BIEN_occurrence_species(species = "Nothofagus pumilio")

# Transformamos a sf
N_pumilio_sf <- N_pumilio %>% st_as_sf(coords = c(3,2), 
                                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#* Cuando uno trabaja con especies uno debe determinar las presencias y además un background
#* Trabajaremos con un polígono y un buffer

# Generar un polígono a partir de los puntos ----
#* (este contiene a todos los puntos originales)
Hull <- N_pumilio_sf %>% 
          # Transformar los point a un multypoint
          st_union() %>% 
          # Tranformarlo a un polígono simple/mínimo convexo
          st_convex_hull()

ggplot() + geom_sf(data = Hull) + geom_sf(data = N_pumilio_sf)

# Buffer ----
buffer <- Hull %>% 
  # Buffer de 1° ~ 111 km
  st_buffer(dist = 1) %>% 
  st_as_sf()

# Base de datos countriesHigh
data ("countriesHigh")

SA <- countriesHigh %>% st_as_sf() %>% st_make_valid() %>% 
  # Cortarlo en base a buffer
  st_crop(buffer)

ggplot() + 
  geom_sf(data = SA) +
  geom_sf(data = N_pumilio_sf)

# Capas bioclimáticas ----
Bioclimatic <- getData(name = "worldclim", var = "bio", res = 0.5, lon = -70, lat = -50) %>% 
  crop(buffer) %>% trim()






