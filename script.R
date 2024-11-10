#paquetes
library(airqualityES)
library(tidyverse)

#carga de datos

zonas_verdes <- pxR::read.px("data/zonas_verdes.px")$DATA

alzheimer <- pxR::read.px("data/alzheimer_total.px")$DATA

data("airqES")

#Modificacion Tablas

'''
Valores Límites de los Contaminantes:
PM10 = 40
PM2,5 = 25
Pb = 0,5
As = 6
Cd = 5
Ni = 20
B(a)P = 1

'''

calidad_aire <- airqES %>%
  filter(year == 2018) %>%
  mutate(media_mensual = apply(.[, c(8:38)], 1, mean, na.rm = TRUE))%>%
  select(month, province, station_id, pollutant, media_mensual) 
