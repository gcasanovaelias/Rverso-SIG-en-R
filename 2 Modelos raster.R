#Packages----
library(rworldxtra)
library(raster)
library(sf)
library(tidyverse)

#Base de datos PREDICTS----
  #Más de tres millones de datos de biodiversidad
data('countriesHigh')
datos <- read_csv('https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Bombus.csv')
  
#Pasar de data frame a un archivo sf----
datos <- datos %>% st_as_sf(coords = c(5,6), #Se indican que las coordenadas son las columnas de longitud y latitud del data frame
                            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") #proyección más básica de todas, se emplea cuando se tienen las coordenadas en longitud y latitud

mapa <- countriesHigh %>% st_as_sf() %>% 
  st_crop(datos) #cortar un shapefile (formato sf)

# ¿Cual es el extent?
st_bbox(datos)

# ¿Cómo ecportar un sf?
dir.create("resultados")
setwd("~/Casanova/Estudio Personal/Rverso/GIS en R_2020/Clase 2 - modelacion en raster/resultados")
write_sf(datos,'datos.shp')

#Graficando----
  #Grafico base
ggplot() + geom_sf(data = mapa) + 
  theme_bw()
  
  #Agreguemos los puntos
ggplot() + geom_sf(data = mapa) + 
  geom_sf(data = datos) + 
  theme_bw()
#la visualización se realiza por capa por lo que el orden de los componentes espaciales sí importa

  #Agreguemos los colores por especie
ggplot() + geom_sf(data = mapa) + 
  geom_sf(data = datos, 
          aes(color = species)) +
  theme_bw()
   #*'Bombus vosnesenskii' es la última capa que entra de las especies por lo que puede sobreponerse a otros puntos que indican otras especies (colores)

  #Agreguemos el tamaño por abundancia
ggplot() + geom_sf(data = mapa) + 
  geom_sf(data = datos, 
          aes(color = species, 
              size = Measurement)) + 
  theme_bw() + 
  theme(legend.position = 'bottom')

colnames(datos)

  #Un panel por cada especie
ggplot() + geom_sf(data = mapa) + 
  geom_sf(data = datos) + 
  facet_wrap(~species) + #función que nos permite obtener un gráfico por una variable especificada (en este caso por especie)
  theme_bw()

#Modelamiento----
#Modelemos la abundancia de Bombus impatiens
B_impatiens <- datos %>% dplyr::filter(species == 'Bombus impatiens')

ggplot() + geom_sf(data = mapa) + 
  geom_sf(data = B_impatiens, 
          aes(size = Measurement)) + 
  theme_bw()

#Obtención de datos climáticos (datos continuos)
bioclim <- getData('worldclim',
                   var = 'bio',
                   res = 10) %>% crop(B_impatiens)
  #*Se obtiene un RasterBrick
  #*worldclim: base de datos climáticas
  #*crop(): se emplea para rasters

plot(bioclim)
 #*Posee 19 capas bioclimaticas pero aqui solo caben 16 en la ventana de plot
 #*Todas las temperaturas esán en grados celcius pero están multiplicadas por 10
 #*https://www.worldclim.org/data/bioclim.html
  #*para saber que variables se indican en las capas bio

#Selección de capas raster dentro de un stack
bioclim = bioclim[[c(1,7,12,15)]]
  #*para seleccionar capas dentro de un brick o stack se debe emplear doble corchete
  #*bio1: temperatura media anual
  #*bio7: variación térmica anual
  #*bio12: precipitación anual
  #*bio15: variación anual de precipitación

#Juntemos las bases de datos----
#Extraccion de datos en los puntos
clima <- raster::extract(bioclim, B_impatiens) %>% 
  as.data.frame()
  #con el extract obtenemos una matriz como resultado, debemos convertirla a un data frame porque las matrices no son tan útiles cuando trabajamos con bases de datos

#Juntamos los datos de abundancia con clima
B_impatiens=B_impatiens %>% bind_cols(clima)
  #IMPORTANTE: tenemos que ocupar bind_cols() y no cbind(),esta última no funciona con formatos sf
colnames(B_impatiens)
class(B_impatiens)

#Modelo----
  #glm(): Generalized Linear Model
fit1=glm(Measurement ~ bio1 + I(bio1^2) + bio7 + I(bio7^2) + 
           bio12 + I(bio12^2) + bio15 + I(bio15^2),
         family = poisson, #estamos trabajando con una base de datos de abundancia donde el valor minimo será 0
         data = B_impatiens)
#Predecir en un raster----
prediccion=predict(bioclim, #Mapa raster que contiene las VI del modelo de manera continua
                   fit1, #Modelo que predice Measurement en base a las VD
                   type = 'response') #Para realizar la predicción directa de abundancia (Measurement) debido a que estamos trabajando con un modelo
  #*Generé un modelo y ahora lo estoy proyectando espacialmente
plot(prediccion,colNA = 'black')

#Mapa con sf----
  #Hacer los mapas con sf permite combinar los raster con los shapefiles

usa=getData(name = 'GADM', country = 'USA',level = 1) %>% 
  st_as_sf() %>% st_crop(B_impatiens)
  #el polígono viene en un formato OGR pero lo necesitamos en sf

pred_df=prediccion %>% as('SpatialPixelsDataFrame') %>%
  as.data.frame() %>% rename(Abundancia = layer)
  #*previo a graficar debemos convertir la capa raster a un data frame
  #*opr default al convertit los datos a un data frame los valores se denominan 'layer', se le cambia el nombre a 'Abundancia'
head(pred_df)
  #Se observa como la variable cambió de nombre a 'Abundacia' y que se crearon las columnas x e y que indican las coordenadas

#Mapa con los límites nacionales
ggplot() + geom_tile(data = pred_df,aes(x = x, y = y, fill = Abundancia)) + 
  geom_sf(data = mapa, alpha = 0, color = 'white') + #alpha=0: el relleno se hace transparente pero el contorno se mantiene visible y se cambia a un color blanco
  theme_bw() + scale_fill_viridis_c() + 
  xlab('') + ylab('')

#Agregar los puntos de medición y escala de estados (no nacional)
  #nos permite ver como son los datos medidos comparados con los predichos por el modelo y observar si tienen sentido
ggplot() + geom_tile(data = pred_df, aes(x = x, y = y, fill = Abundancia)) + #mapa raster de base
  geom_sf(data = usa, alpha = 0, color = 'white') + #limites nacionales (vector)
  geom_sf(data = B_impatiens, aes(size = Measurement)) + #puntos de medicion (vector)
  theme_bw() + scale_fill_viridis_c(option = 'plasma') + #otra opción de colores dentro de la escala viridis
  theme(legend.position = 'bottom') +
  xlab('') + ylab('')

#Predicción al futuro----
  #getData() permite obtener bases de datos con pronósticos
futuro=getData('CMIP5', #proyecciones de cambio climático
               var='bio', #mismas variables bioclimaticas
               res=10,
               rcp=85, #hace referencia al escenario de emisión de CO2, es uno de los escenarios con emisiones más altas
               model='HD', #hay muchos modelos que predicen el cambio climático, aquí emplearemos el HD
               year=70) %>% crop(B_impatiens) #Seleccionamos las predicciones hasta el 2070
futuro = futuro[[c(1,7,12,15)]]
plot(futuro)
names(futuro) 
  #*los nombres cambiaron harto
  #*IMPORTANTE: las variables tienen que tener los mismos nombres que las variables del modelo original
names(futuro)=c('bio1','bio7','bio12','bio15')
futuro_pred=predict(futuro,
                    fit1,
                    type = 'response') #respuesta de la variable dependiente
plot(futuro_pred,colNA = 'black')
