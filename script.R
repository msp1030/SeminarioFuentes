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


alzheimer_df<-alzheimer$value
alzheimer_df<-alzheimer_df[,-c(1,2)]#eliminamos las columnas 1 y 2 

print(colnames(alzheimer_df))



#pivotar zonas verdes
wide_zonas_verdes<-
  zonas_verdes_df%>%
  pivot_wider(names_from = "Nivel.de.satisfacción", values_from = "value")




#alzheimer_zonas_verdes<-left_join(x = alzheimer_df, y = wide_zonas_verdes, by = c("Comunidades.y.Ciudades.Autónomas")) 



#creamos un vector con los nombres de los meses
meses<-c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
         "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
#remplazamos los meses en la tabla
calidad_aire$month<-meses[calidad_aire$month]


#creamos un vector con los nombres de las provincias
provincia = c("Alava","Albacete","Alicante","Almeria","Avila", "Badajoz", "Baleares", "Barcelona", "Burgos", "Caceres", "Cadiz", "Castellon","Ciudad Real","Cordoba","Coruña","Cuenca","Girona","Granada","Guadalajara","Gipuzkoa","Huelva","Huesca","Jaen","Leon","Lleida","Rioja","Lugo","Madrid","Málaga","Murcia","Navarra","Ourense","Asturias","Palencia","Palmas","Pontevedra","Salamanca","Santa_Cruz_de_Tenerife","Cantabria","Segovia","Sevilla","Soria","Tarragona","Teruel","Toledo","Valencia","Valladolid","Bizkaia","Zamora","Zaragoza")
factor(provincias)



calidad_aire$province<-provincia[calidad_aire$province]

print(calidad_aire)

wide_calidad_aire<-
  calidad_aire%>%
  pivot_wider(names_from = "month", values_from = "media_mensual")


#agrupar meses
wide_calidad_aire<-
  wide_calidad_aire%>%
  mutate(media_anual = apply(.[, c(4:15)], 1, mean, na.rm = TRUE))


#quitamos los datos por meses
wide_calidad_aire<-wide_calidad_aire[,-c(4:15)]

#quitamos los datos a de ceuta, melilla y los de nivel nacional de alzheimer_xonas_verdes




view(wide_calidad_aire)
view(alzheimer_df)
view(wide_zonas_verdes)
#eliminamos algunas columnas de valoracion
wide_zonas_verdes<-wide_zonas_verdes[,-c(2:5)]

alzheimer_zonas_verdes<-left_join(x = alzheimer_df, y = wide_zonas_verdes, by = c("Comunidades.y.Ciudades.Autónomas")) 
view(alzheimer_zonas_verdes)
alzheimer_zonas_verdes<-alzheimer_zonas_verdes[-c(1),]
view(alzheimer_zonas_verdes)

wide_calidad_aire <- wide_calidad_aire %>%
  mutate(
    porcentaje = case_when(
      !is.na(media_anual) & pollutant == "PM10" ~ (media_anual / 40) * 100,
      !is.na(media_anual) & pollutant == "PM2.5" ~ (media_anual / 25) * 100,
      !is.na(media_anual) & pollutant == "Pb" ~ (media_anual / 0.5) * 100,
      !is.na(media_anual) & pollutant == "As" ~ (media_anual / 6) * 100,
      !is.na(media_anual) & pollutant == "Cd" ~ (media_anual / 5) * 100,
      !is.na(media_anual) & pollutant == "Ni" ~ (media_anual / 20) * 100,
      !is.na(media_anual) & pollutant == "B(a)P" ~ (media_anual / 1) * 100,
      TRUE ~ NA_real_
    )
  )


View(wide_calidad_aire)

