#paquetes
library(airqualityES) #paquete para trabajar la calidad del aire en España
library(tidyverse)#paquete para poder manipular y visualizar los datos
library(pxR)
library(dplyr)
#carga de datos

zonas_verdes <- read.px("data/zonas_verdes.px")$DATA#carga un archivo con datos sobre las zonas verdes

alzheimer <- read.px("data/alzheimer_total.px")$DATA#carga un achivo con datos sobre el alzheimer

data("airqES")#carga el conjunto de datos con información sobre la calidad del aire en España

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
# añadimos una columna con valor "Si" cuando excede el limite y "No" en caso contrario
calidad_aire <- calidad_aire%>%
  mutate(exceso_limite=case_when(
    pollutant == "PM10" & media > 40 ~ "Sí"
    pollutant == "PM2,5" & media > 25 ~ "Sí"
    pollutant == "Pb" & media > 0,5 ~ "Sí"
    pollutant == "AS" & media > 6 ~ "Sí"
    pollutant == "Cd" & media > 5 ~ "Sí"
    pollutant == "Ni" & media > 20 ~ "Sí"
    pollutant == "B(a)P" & media > 1 ~ "Sí"
    True ~"No"
  ))
'''

calidad_aire <- airqES %>%
  filter(year == 2018) %>%#filtra el airqES para el año 2018
  mutate(media_mensual = apply(.[, c(8:38)], 1, mean, na.rm = TRUE))%>%#creamos una columna llamada media_mensual, se calcula la media de las columnas 8-38 por cada fila (evitando los valores NA) 
  select(month, province, station_id, pollutant, media_mensual)#se filtran las columnas month, province, station_id, pullutant y media_mensual

#quitamos los valores de zonas_verdes que no se centran en provincias
zonas_verdes_df <- zonas_verdes$value
zonas_verdes_df<-zonas_verdes_df[-c(1,2,3,4,5), ]#eliminamos las filas 1, 2, 3, 4 y 5
zonas_verdes_df

print(colnames(zonas_verdes_df))

alzheimer_df<-alzheimer$value
alzheimer_df<-alzheimer_df[,-c(1,2)]#eliminamos las columnas 1 y 2 
alzheimer_df
print(colnames(alzheimer_df))



#pivotar zonas verdes
view(zonas_verdes_df)
wide_zonas_verdes<-
  zonas_verdes_df%>%
  pivot_wider(names_from = "Nivel.de.satisfacción", values_from = "value")
view(wide_zonas_verdes)
view(calidad_aire)




alzheimer_zonas_verdes<-left_join(x = alzheimer_df, y = wide_zonas_verdes, by = c("Comunidades.y.Ciudades.Autónomas")) 
view(alzheimer_zonas_verdes)


#creamos un vector con los nombres de los meses
meses<-c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
         "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#remplazamos los meses en la tabla
calidad_aire$month<-meses[calidad_aire$month]
view(calidad_aire)

#creamos un vector con los nombres de las provincias
provincias<-c("Álava", "Albacete", "Alicante", "Almería", "Asturias", "Ávila", 
                "Badajoz", "Barcelona", "Burgos", "Cáceres", "Cádiz", "Cantabria", 
                "Castellón", "Ciudad Real", "Córdoba", "Cuenca", "Girona", "Granada", 
                "Guadalajara", "Gipuzkoa", "Huelva", "Huesca", "Islas Baleares", 
                "Jaén", "La Rioja", "Las Palmas", "León", "Lleida", "Madrid", 
                "Málaga", "Murcia", "Navarra", "Ourense", "Palencia", "Pontevedra", 
                "Salamanca", "Santa Cruz de Tenerife", "Segovia", "Sevilla", "Soria", 
                "Tarragona", "Teruel", "Toledo", "Valencia", "Valladolid", "Vizcaya", 
                "Zamora", "Zaragoza")
calidad_aire$province<-provincias[calidad_aire$province]
view(calidad_aire)




