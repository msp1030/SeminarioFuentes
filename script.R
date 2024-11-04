#paquetes
library(airqualityES)
library(tidyverse)

#conjuntos de datos

data("airqES")
#View(airqES)

airqES[]

calidad_aire <- airqES %>%
  filter(year == 2018) %>%
  mutate(media_dias = apply(.[, c(8:38)], 1, mean, na.rm = TRUE))%>%
  select(month, province, station_id, pollutant, media_dias) 