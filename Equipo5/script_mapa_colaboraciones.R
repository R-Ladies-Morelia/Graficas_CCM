#----
# Crear Mapa de colaboraciones internacionales de estudiantes

#----
# Primero parte: extraer las coordenadas de cada país
library(readxl)
library(tidygeocoder)
library(tidyverse)
# Cargar base de datos con paises de colaboraciones
datos <- read_excel("graf-ccm-rladies/datos.xlsx")

# Convertir a tibble para obtener las coordenadas de cada país
paises <- datos %>%
  select(Pais, Num_Articulos) %>%
  as_tibble()

# Geocodificar las regiones
lat_longs <- paises %>%
  geocode(Pais, method = 'osm', lat = latitude , long = longitude)

## NOTA: República Checa tiene erronea la coordenada y se corrigió manualmente.

# El archivo se guardo como lat_longs.csv para después solo cargarlo.

#----
# Segunda parte: Crear data frame con las primeras dos columnas las coordenadas de México en todas las filas,
# y en la tercera y cuarta columna las longitudes y latitudes de los países con
# los que se ha colaborado. Las columnas son las siguientes:
#long_from | lat_from | long_to | lat_to

# Falta código ^^'

#----
# Tercera parte: Crear el mapa

library(readr)
library("ggplot2")
library("ggspatial")
library("sf")
library("tmap")
library("rnaturalearth")
library("rnaturalearthdata")

# Fijar la ruta:
#setwd("~/graf-ccm-rladies")

#Leer los dos data frames
# data frame con las columnas long_from | lat_from | long_to | lat_to de la segunda parte
df <- read_csv("datos_lat_long1.csv")
# data frame con las longitudes y latitudes de la primera parte
lat_longs <- read_csv("datos_lat_long.csv")


# Usando la librería rnaturalearth se crea el archivo sf que se usará para crear el mapa base.
world <- ne_countries(scale = "medium", returnclass = "sf")#baja datos paises

#se usa geom_sf para crear el mapa base usando el dataframe world

ggplot(data = world)+
  geom_sf(aes(fill = continent, alpha=0.5))+
  geom_point(data=lat_longs, aes(longitude, latitude),
             color= "#0047ab", size=2.5)+ # en geom_point se usa el data frame de la primera parte
  geom_curve(data=df, aes(x=long_from, y=lat_from, xend=long_to, yend=lat_to),
             arrow=arrow(angle=15,ends="last",length=unit(0.3,"cm"),type="closed"),color="#002e63", alpha=0.8, curvature = 0.15)
# en geom_curve se usa el data frame de la segunda parte para marcar las flechas de colaboraciones

