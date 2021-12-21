#Packages----
install.packages('gstat')
library(gstat)
library(raster)
library(rworldxtra)
library(sf) #simple features
library(sp) #se instala en conjunto con rgdal. sp es la libreria que trabaja con el formato 'SpatialPointsPolygons' por ejemplo
library(tidyverse)

#1. Pre trabajo----
  #Cargar los datos de meuse----
  #datos en formato dataframe de concentracion de metales pesados, su cercania a un rio om y el uso de suelo
data(meuse)
class(meuse)

  #Transformamos a sf----
Meuse = st_as_sf(meuse, 
                 coords = c(1, 2),
                 crs = 28992) #ó '+init=epsg:28992' (entre comillas)
 
class(Meuse)

  #Transformamos a sp (SpatialPointsDataframe)----
coordinates(meuse) = ~x + y
class(meuse)
  #ahora la clase de meuse esta en otro formato solo con emplear la funcion coordinates()

#¿cual es la diferencia entre los formataos sp y sf?
View(Meuse) #en el formato sf puedo seguir observando los datos como df (emplearse en el tidyverse)
View(meuse) #no todas las herramientas aceptan el formato sf, aún

  #Graficamos concentracion de Zn----
ggplot() + geom_sf(data = Meuse, aes(color = zinc)) + 
  scale_color_viridis_c() + 
  theme_bw()

#2. Modelos de interpolacion----
  #Interpolacion de valores en pixeles desconocidos en base a distancia
  #Por ejemplo, es util emplear modelos de interpolacion cuando trabajamos con datos de estaciones meteorologicas
  #objetivo: predecir cual va ser el valor entre los puntos de la variable de interes

  #Semi-variograma----
  #*Modelo nulo
  #*Que tanto varían los valores según la distancia
  #*medida de la variacion de la variable de interes de acuerdo a la distancia
  #*cuanto más distancia tenemos mayor será la variacion (semivarianza) de los valores (Primera ley de la geografia). Es decir, en la medida que los puntos se encuentren más cercanos entre sí sus valores tienden a ser homogeneos

Z_vgm_null = gstat::variogram(log(zinc) ~ 1, meuse) #modelo nulo

  #*log() se emplea para que los modelos construidos posteriormente tengan siempre valores positivos, si emplearamos la variable zinc sin ninguna transformacion obtendriamos en algunos casos valores negativos

ggplot() + geom_point(data = Z_vgm_null, aes(x = dist, y = gamma))
  
#*Interpretacion de los valores obtenidos
  #*np se refiere a los pares de puntos
  #*#*gamma hace referencia a una semivarianza
    #*57 pares de puntos presentan una distancia de 79.29 metros entre si y estos van a tener una semivarianza (gamma) de 0.12344
  
  #*Cuando la linea se vuelve asíntotica eso quiere decir que la vairacion ya no es producto de la distancia entre los puntos sino que existen otros factores ambientales que inciden en ella

#Al variograma podemos agregarle otras variables que tenemos en la base de datos para ir eliminando esa varianza (en el modelo nulo se presenta toda la)

    #2.1. Modelo nulo----
  #tenemos toda la varianza

Z_vgm_null = variogram(log(zinc) ~ 1, meuse) %>% 
  # Agregarle la columna "Modelo"
  mutate(Modelo = 'Nulo')

ggplot() + geom_point(data = Z_vgm_null, aes(x = dist, y = gamma))

variogram(log(zinc) ~ 1, meuse, cutoff = 300) %>% #el parametro cutoff es sensible al valor indicado (distancia), mientras se observe al comportamiento asintotico de los datos esta todo relativamente bien
  mutate(Modelo = 'Nulo') %>% plot()

    #2.2. Modelo Espacial----
  #Agregar la componente espacial al modelo

Z_vgm_Spat = variogram(log(zinc) ~ x + y, meuse) %>% 
  mutate(Modelo = 'Espacial')

ggplot() + geom_point(data = Z_vgm_Spat, aes(x = dist, y = gamma))

  #*la varianza bajó
  #*la varianza en relacion a las coordenadas es una parte de la varianza total

    #2.3. Modelo distancia----

Z_vgm_Dist = variogram(log(zinc) ~ dist, meuse) %>% 
  mutate(Modelo = 'Distancia')

ggplot() + geom_point(data = Z_vgm_Dist, aes(x = dist, y = gamma))

    #2.4. Modelo sqrt distancia----
  #algunos fenomenos como la concentracion empiezan a disminuir no-linealmente, disminuyen de una manera más acelerada 

Z_vgm_Dist_sq = variogram(log(zinc) ~ sqrt(dist), meuse) %>% 
  mutate(Modelo = 'sqrt dist')

ggplot() + geom_point(data = Z_vgm_Dist_sq, aes(x = dist, y = gamma))

    #Union de modelos----

Z_vgm = list(Z_vgm_Dist, Z_vgm_null, Z_vgm_Spat, Z_vgm_Dist_sq) %>%
  purrr::reduce(bind_rows) #recordar que reduce() agrupa elementos de una lista según una función

ggplot() + 
  geom_point(data = Z_vgm, aes(x = dist, y =gamma, color = Modelo)) + 
  geom_line(data = Z_vgm, aes(x = dist, y =gamma, color = Modelo)) + 
  theme_bw()

  #Fit del variograma----
Abn_fit_Spat = fit.variogram(Z_vgm_Spat, 
                             model = vgm(psill = 1, #vgm() hace referencia a un variograma
                                         model = 'Sph', #tipo de correlación, en este caso esférica
                                         range = 700, 
                                         nugget = 1))
#*Este fit busca encontrar el punto donde se inicia el comportamiento asintotico (a que distancia y que valor de gamma) 

#*IMPORTANTE: los puntos no fueron arbitrarios, son basados en las estimaciones visuales de los graficos anteriores y le indican a la función fit desde que valores empezar a calcular
  #*esto es un modelo no lineal; siempre requieren de puntos iniciales de donde empezar
  #*es recomendable ocupar los mismos puntos de inicio para todos los modelos estimados,
    #*debido a esto es que los puntos de inicio sean valores intermedios entre las gráficas observadas

#*range = 1098.571: a esa distancia es a la cual llegamos a esa asintota
#*psill = (0.38866509 + 0.08234213) = ~0.47: la semivaianza estabilizada a la asíntota
#*nugget: varianza esperada a distancia cero (tiene que ver con el intercepto)





