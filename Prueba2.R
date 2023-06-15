
#librería
library(readxl)
library(tidyr)

#Ver en qué directorio estamos parados
getwd()

#Leer el excel de nuestra data
data <- read_excel("dataprueba.xlsx")

#Cambiar el encabezado de la tabla a:
data<- data %>% rename(valor = value) %>% rename(tipodevalor = Tipodevalor) %>% rename(paises = País__ESTANDAR) %>% rename(anio = Años__ESTANDAR)

#Seleccionando las variables de interés y ordenar por anio
data <- data %>% select(paises, anio,valor, tipodevalor) %>% arrange(anio)

#Quitar los na de la tabla
data <- na.omit(data)

#Redondear los valores a dos decimales
data$valor<- round(data$valor, 2)
