#Packages----
install.packages('leaflet')
install.packages('leaflet.extras')
install.packages('mapedit')
library(leaflet)
library(leaflet.extras)
library(mapedit)
library(sf)
library(rgdal)
library(tidyverse)
library(raster)
library(rasterVis)
library(rworldxtra)
library(mapview)

#1.Leaflet----
  #libreria de JavaScript que permite generar mapas interactivos en base a html,
    #Esta libreria es manipulable en R, no todo es pefectamente manipulable
    #html es el formato en el que estan las presentaciones del curso, podemos verlas desde el navegador
setwd("~/Casanova/Estudio Personal/Rverso/GIS en R/Clase 3 - Mapas Interactivos en leaflet")

nothofagus = read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Nothofagus.csv")
glimpse(nothofagus)

  #1.1.Graficar ----
osp = leaflet() %>% leaflet::addTiles() #Agregar OpenStreetMap como mapa base
  
  #Opciones
  osp %>% leaflet::addCircles(data = nothofagus, lat = ~latitude, lng =  ~longitude) #Agregar los puntos delos datos nothofagus
  
  osp %>% leaflet::addAwesomeMarkers(data = nothofagus, lat = ~latitude, lng = ~longitude) #Agregar marcadores
  
  osp %>% leaflet::addCircleMarkers(data = nothofagus, lat = ~latitude, lng = ~longitude) #Agregar circulos más grandes y transparentes

  #1.2.Cambiemos el color según la especie----
#¿Cuantas especies están en los datos?
number_spp = nothofagus$scrubbed_species_binomial %>% 
    unique() %>% 
    length()

#¿Cual es el nombre de las especies?
spp_names = nothofagus$scrubbed_species_binomial %>% unique()

#Colores
#https://colorbrewer2.org/#type=qualitative&scheme=Paired&n=9
colores = rainbow(9)

#Creamos la paleta de colores
pal = leaflet::colorFactor(colores, 
                           domain = spp_names) #los valores que van a tomar esos colores
  #colorFactor(): generar una paleta de colores para variables categoricas (factores)
  #se genera una función

leaflet() %>% addTiles() %>% 
  addCircles(data = nothofagus, 
             lat = ~latitude, 
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.7)

  #1.3.Identificación de los colores----  
    #Labels----
leaflet() %>% addTiles() %>%
  addCircles(data = nothofagus,
             lat = ~latitude,
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.7,
             label = ~date_collected) #labels: nos da información (de una columna) cuando posamos en los puntos

    #Popups----
m = leaflet() %>% addTiles() %>%
  addCircles(data = nothofagus,
             lat = ~latitude,
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.7,
             label = ~date_collected,
             popup = ~datasource, #información que aparece cuando apretas los puntos
             group = 'Especies') #grupo que vamos a ocupar posteriormente para tener control de ciertas areas del mapa

    #Leyenda----
m = m %>% addLegend(data = nothofagus, 
                position = 'bottomright', 
                pal = pal, #paleta de colores empleada anteriormente
                values = ~scrubbed_species_binomial, #valores de los colores
                title = 'Especies',
                opacity = 0.7,
                group = 'Leyenda')

  #1.4.Control de objetos----
#Selección de datos, poner o sacar capas según lo que necesite
#Ahora sí se utilizan los grupos anteriormente creados
m %>% addLayersControl(overlayGroups = c('Especies', 'Leyenda'), #que grupos voy a permitir controlar
                       options = layersControlOptions(collapsed = T)) #si las opciones de capa estan colapsadas en un boton

  #1.5.Grupos por especie----
  #Poder visualizar c/ especie por si misma;tener un control de poder agregar o sacar del mapa
  #Generar una capa por c/ especie
which(spp_names == 'Nothofagus alpina')

n_alpina = nothofagus %>% 
  dplyr::filter(scrubbed_species_binomial == spp_names[2])

leaflet() %>% addTiles() %>%
  addCircles(data = n_alpina,
             lat = ~latitude,
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.7,
             label = ~date_collected,
             popup = ~scrubbed_species_binomial, #información que aparece cuando apretas los puntos
             group = 'alpina')
  #Les sigue asignando colores diferentes debido al parametro color que esta diferenciando para c/ especie
  #Ocupar herramientas programéticas para realizar tareas más eficientemente

    #Loop para todas las especies----
spp_pres = list() #genero una lista vacía primero
for (i in 1:length(spp_names)){
  spp_pres[[i]] = nothofagus %>% 
    filter(scrubbed_species_binomial == spp_names[i])
}
names(spp_pres) = spp_names #le cambio el nombre a los elementos de la lista

#Para un mapa
spp_map = leaflet() %>% addTiles() %>%
  addCircles(data = spp_pres[[1]],
             lat = ~latitude,
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.5,
             label = ~scrubbed_species_binomial,
             popup = ~datasource,
             group = spp_names[1]) %>%
  addCircles(data = spp_pres[[2]],
             lat = ~latitude,
             lng = ~longitude,
             color = ~pal(scrubbed_species_binomial),
             fillOpacity = 0.5,
             label = ~scrubbed_species_binomial,
             popup = ~datasource,
             group = spp_names[2])

    #Mejor trabajar con loop tambien...----
#Agregar capas
spp_map = leaflet() %>% addTiles() #primero dejo una misma base (mapa OSM)
for(i in 1:length(spp_pres)){
  spp_map = spp_map %>% 
    addCircles(data = spp_pres[[i]],
               lat = ~latitude,
               lng = ~longitude,
               color = ~pal(scrubbed_species_binomial),
               fillOpacity = 0.5,
               label = ~scrubbed_species_binomial,
               popup = ~datasource,
               group = spp_names[i])
}

#Agregar leyenda
spp_map = spp_map %>% 
  addLegend(data = nothofagus,
            position = 'bottomright',
            pal = pal,
            values = ~scrubbed_species_binomial,
            title = 'Especies',
            opacity = 0.7,
            group = 'Leyenda')

#Agregar control por especie
grupos = c('Leyenda', spp_names) #ahora tenemos 10 grupos para ejercer control sobre ellos; la leyenda y las 9 especies
  #opción 1 (todos los grupos en un overlay)
spp_map = spp_map %>% 
  addLayersControl(overlayGroups = grupos,
                   options = layersControlOptions(collapsed = T)) %>%
  hideGroup('Leyenda')

  #opción 2 (las espcies con botones o en grupos base)
spp_map = spp_map %>% 
  addLayersControl(overlayGroups = 'Leyenda',
                   baseGroups = spp_names, #solamente me va a permitir escoger 1
                   options = layersControlOptions(collapsed = T)) %>%
  hideGroup('Leyenda')

  #1.6.Densidad por especie----
heat_map = leaflet() %>% addTiles()
for(i in 1:length(spp_pres)){
  heat_map = heat_map %>% 
    addHeatmap(data = spp_pres[[i]],
               lat = ~latitude,
               lng = ~longitude,
               group = spp_names[i],
               blur = 50,
               radius =20)
}
heat_map = heat_map %>% 
  addLayersControl(baseGroups = spp_names,
                   options = layersControlOptions(collapsed = F))

#2. Polígonos en leaflet----

#Descargar archivos de internet
download.file("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Chile.zip",
              destfile = "Chile.zip") #Nombre del archivo

#Descomprimir archivos
unzip('Chile.zip')

chile = sf::st_read('Chile.shp')
plot(chile)

#Los sf no son adecuados para leaflet por lo que se deben convertir
chile_spat = chile %>% as_Spatial() #SpatialPOlygonDataFrame

  #2.1.Graficando en leaflet----
  #Solo los poligonos
leaflet(chile_spat) %>% addTiles() %>% 
  addPolygons()

regiones_chile = leaflet() %>% addTiles() %>% 
  addPolygons(data = chile_spat,
              fillColor = topo.colors(16, alpha = NULL), weight = 0.5,
              label = ~NAME_1, 
              group = 'Regiones') %>%
  addLayersControl(overlayGroups = 'Regiones',
                   options = layersControlOptions(collapsed = T))

  #Con los puntos de nothofagus
leaflet() %>% addTiles() %>% 
  addCircles(data = nothofagus,
             lat = ~latitude,
             lng = ~longitude) %>%
  addPolygons(data = chile_spat,
              fillColor = topo.colors(16, alpha = NULL), weight = 0.5,
              label = ~NAME_1, 
              group = 'Regiones') %>%
  addLayersControl(overlayGroups = 'Regiones',
                   options = layersControlOptions(collapsed = T))

  #2.2.Agreguemos medidas de superficie----
#para c/ poligono se visualiza su area en km2
regiones_chile = regiones_chile %>%
  addMeasurePathToolbar(options = measurePathOptions(imperial = F, #las medidas imperiales son en acres
                                                     minPixelDistance = 100,
                                                     showDistances = F))

  #2.3.Generar nuevas áreas----
#Permite dibujar nuevas áreas
regiones_chile = leaflet() %>% addTiles() %>%
  addPolygons(data = chile_spat,
              fillColor = topo.colors(16, alpha = NULL),
              weight = 0.5,
              label = ~NAME_1,
              group = 'Regiones') %>%
  addDrawToolbar(targetGroup = 'Regiones Marinas', #herramienta que nos permite dibujar en el mapa
                 editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
  addLayersControl(overlayGroups = c('Regiones', 'Regiones Marinas'),
                   options = layersControlOptions(collapsed = T)) %>%
  addMeasurePathToolbar(options = measurePathOptions(imperial = F,
                                                     minPixelDistance = 100,
                                                     showDistances = T)) #para mostrar el largo de las lineas
    #A los nuevas dibujos se les agrupa en un grupo diferente al de regiones, en este caso se llama regiones marinas
#2.4. Editar estilos----
  #modificar los colores, las lineas, entre otras cosas
regiones_chile = regiones_chile %>% addStyleEditor()

#3. ¿Como lo comparto?----
#RMarkDown

#4. mapedit----
  #Cambio de vuleta a formato sf
chile_sf = chile_spat %>% st_as_sf()

nuevas_regiones = mapview::mapview(chile_sf) %>%
  mapedit::editMap('chile_sf')
  #hace varias cosas automatizadas

#Permite incorporar los vectores creados a R
nuevas_regiones$drawn

nuevas_regiones$drawn %>% plot()

nuevas_regiones$drawn %>% write_sf('lineas.shp')