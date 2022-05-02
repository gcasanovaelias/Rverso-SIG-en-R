# Packages ----
library(tidyverse)
library(raster)
library(ggforce)
library(ggrepel)
library(scales)
library(rworldxtra)
library(sf)
library(forcats)

# Base de datos
githubURL <- ("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/Presentaciones_Espacial/Casos_Activos_Valpo.rds")

# Descarga
download.file(githubURL, "Casos_Activos_Valpo.rds") #Se descarga en el directorio actual

# Leer
casos_activos_valpo <- readRDS("Casos_Activos_Valpo.rds")

# Graficar ----
# Gráfico simple
ggplot() + geom_sf(data = casos_activos_valpo, aes(fill = Activos_por_100.000)) +
  facet_wrap(~Fecha) +
# Modificar la escala
  scale_fill_gradientn(name = "Activos por 100.000 hab", #La leyenda cambia de nombre
                       colours = rev(heat.colors(10))) #Otras escalas de colores: rainbow, terrain.colors, topo.colors, cm.colors, heat.colors

# Escala viridis (continua)
plot1 <- ggplot() + geom_sf(data = casos_activos_valpo, 
                            size = 0.05, #lineas más atenuadas
                            aes(fill = Activos_por_100.000)) + 
  facet_wrap(~Fecha)

plot1 + scale_fill_viridis_c(name = "Activos por 100.000 hab")

# Cambiar la dirección
plot1 + scale_fill_viridis_c(name = "Activos por 100.000 hab",
                             direction = -1)

# Discretizar los valores (binned)
plot1 + scale_fill_binned(name = "Activos por 100.000 hab")

# Cambiando el número de escalas en los valores discretizados
plot1 + scale_fill_binned(name = "Activos por 100.000 hab", breaks = seq(0, 200, by = 25))
  
# Escala gradient2
  # La escala gradient 2 combina 2 gradientes de colores
plot2 <- plot1 + scale_fill_gradient2(mid = "white", #color del punto medio
                                      # muted(): función del paquete scales que permite bajarle el brillo a los colores
                                      low = muted("blue"),#color del punto bajo
                                      high = muted("red"), #color del punto alto
                                      midpoint = median(casos_activos_valpo$Activos_por_100.000), #valor considerado como medio
                                      name = "Activos por 100.000 hab")

# Ultimos toques
mapa <- plot2 + 
        # titulo, subtitulo y fuente
        labs(title = "Prevalencia de Región de Valparaíso por Provincia",
             subtitle = paste("Actualizado", max(casos_activos_valpo$Fecha)),
             caption = "Datos: https://github.com/MinCiencia/Datos-COVID19",
             y = NULL,
             x = NULL) +
        #tema blanco y negro
        theme_bw() +
        # posición de la leyenda
        theme(legend.position = "bottom")

# Agregar puntos ----
download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10400/2/Toponimos.zip", 
              destfile = "Topo.zip")

# Lo descomprimimos
unzip("Topo.zip")

# Leemos el shapefile y nos quedamos sólo con la región de valparaíso
topo <- read_sf("Toponimos.shp") %>% 
  filter(Region == "DE VALPARAISO") %>% 
  st_crop(casos_activos_valpo)

# Filtros
  # Centros poblados
topo2 <- topo %>% filter(Clase_Topo == "Centro Poblado")
  # Centros poblados que sean capitales provinciales (¡dos filtros distintos en la misma función!)
topo2 <- topo %>% filter(Nombre %in% c("Los Andes",
                                       "Quilpué",
                                       "La Ligua",
                                       "Quillota",
                                       "San Antonio",
                                       "San Felipe",
                                       "Valparaíso"),
                         Clase_Topo %in% c("Centro Poblado"))

# Mapear ----
# Agregar nombres a los puntos. Texto general.
  # Opción 1: geom_sf_text()
mapa + geom_sf(data = topo2) +
  geom_sf_text(data = topo2,
               aes(label = Nombre))
  
  # Opción 2: ggrepel::geom_text_repel(). Repulsión entre los textos y puntos (se ocupa para otros tipos de gráficos)
mapa + geom_sf(data = topo2) +
  ggrepel::geom_text_repel(data = topo2,
                           aes(label = Nombre,
                               geometry = geometry),
                           stat = "sf_coordinates",
                           force = 1)

  # Opción 3: Label. ggrepel::geom_label_repel() 
mapa + geom_sf(data = topo2) +
  ggrepel::geom_label_repel(data = topo2,
                           aes(label = Nombre,
                               geometry = geometry),
                           stat = "sf_coordinates",
                           force = 1)

# Exportar archivos de alta calidad----
#*Generalmente, la exportación normal de imágenes mediante R es de una resolución muy baja (~72 pixeles por pulgada)
#*Para journals y publicaciones se requieren de al menos 300 pixeles por pulgada
#*Debido a esto, se emplean los graphic device (tiff(), png(), pdf())

tiff("Mapa.tiff", 
     units = "in",
     width = 10,
     height = 5,
     res = 600,
     compression = "lzw") #Cuando se descomprime no van a perder información

# Una vez corrido este codigo se debe correr otro set de codigo que genere un nuevo grafico, seguido de dev.off()
plot()

dev.off()

# Delete files with base::unlink()
base::unlink("Mapa.tiff")

# Método ggsave(). Convenient function for saving a plot. It defaults to saving the last plot that was displayed using the size of the current graphics device

ggsave(filename = "Mapa.png",
       plot = last_plot(),
       device = "png",
       path = "../",
       width = 10,
       height = 5,
       units = "in",
       # Plot resolution "retina" (320)
       dpi = "retina",
       # Background colour
       bg = NULL)

# Rasters ----
download.file("https://ndownloader.figshare.com/files/21843771", 
              destfile = "Priority.tiff")

priority <- stack("Priority.tiff")

download.file("https://archive.org/download/priority...", 
              destfile = "Priority.rds")

priority <- readRDS("Priority.rds")

plot(priority, colNA = "black")

# Polígonos
data("countriesHigh")
SA <- countriesHigh %>% 
  st_as_sf() %>% #transformar el shp a sf
  st_make_valid() %>% #algunos shapes presentan problemas cuando se transforman (se cortan)
  st_crop(extent(priority))

ggplot() + geom_sf(data = SA)

# Transformar raster valores NA a "0" en los datos
  # values() y getValues() permiten obtener los datos geoespaciales en modo de vector
values(priority)
getValues(priority)

  # Cambiamos los NA a 0
values(priority) <- ifelse(is.na(values(priority)), #reasignación
                           0, #si los valores son NA se transforman a 0
                           values(priority)) #o se mantienen

  # Generación de una máscara
mask <- rasterize(SA, #polígono a rasterizar
                  priority, #raster empleado como molde
                  field = 1) #raster resultante con valores de 1

  # Álgebra de raster
priority = mask*priority #cuando los NA se multiplican el resultado es NA

plot(priority, colNA = "black")

  # Achicamos el extent
priority <- raster::trim(priority)

# Transformamos las prioridades en categorías
  #* valores de 1 son el 17%, valores de 0.87 son el 30% y valores de 0 son lugares muy desarrollados (agriultura o ciudad)

priority2 <- priority %>% 
  # Transformar los datos de sf a data frame
  as("SpatialPixelsDataFrame") %>% as.data.frame() %>% 
  # Cambiar el nombre de la capa resultante del df
  rename(Priority = layer) %>%
  # Cambiar una variable continua a categórica (case_when())
  mutate(Priority = case_when(Priority >= 0.97 ~ "Prioridad muy alta",
                              Priority < 0.97 & Priority > 0.87 ~ "Prioridad alta",
                              Priority == 0 ~ "Altamente desarrollado")) %>% 
  # Lo que no se encuentre categorizado (de 0 a 0.87) se transforma en NA por lo que se debe sacar
  dplyr::filter(!is.na(Priority)) %>% 
  # Cambiar el orden de los factores
  mutate(Priority = fct_relevel(Priority, 
                                "Prioridad alta", #Lo que va primero
                                "Prioridad muy alta")) #Lo que va despues. Aquello que no se menciona va al último por default.

table(priority2$Priority)

# geom_raster()
P <- ggplot() + geom_sf(data = SA, size = 0.1) +
  geom_raster(data = priority2,
              aes(x = x, y = y, fill = Priority))

# Mejoremos los colores
P <- P + scale_fill_manual(name = "Priority",
                           values = c("#006d2c", "#31a354", "#d7c29e"))

# Otros
P <- P +ylab("") + xlab("") +
  # Cambio de tema
  theme_bw()

# INSET MAP ----
  # ggforce::facet_zoom() es parecido al facet_wrap()
P + facet_zoom(xlim = c(-87.117, -82.56),
               ylim = c(5.51, 11.21),
               horizontal = T,
               zoom.size = 0.8,
               shrink = F)

tiff("Priority.tiff",
     uints = "in",
     width = 10,
     height = 8,
     res = 600,
     compression = "lzw")

dev.off()






