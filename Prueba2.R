
#librería
library(readxl)
library(tidyr)
library(tidyverse)

#Ver en qué directorio estamos parados
getwd()

#Leer el excel de nuestra data
data <- read_excel("BaseDeDatosOriginal.xlsx")

#Cambiar el encabezado de la tabla a:
data<- data %>% rename(valor = value) %>% rename(tipodevalor = Tipodevalor) %>% rename(paises = País__ESTANDAR) %>% rename(anio = Años__ESTANDAR)

#Seleccionando las variables de interés y ordenar por anio
data <- data %>% select(paises, anio,valor, tipodevalor) %>% arrange(anio)

#Quitar los na de la tabla
data <- na.omit(data)

#Redondear los valores a dos decimales
data$valor<- round(data$valor, 2)

##Comparacion entre los dos tipos de valores que tenemos
# Primer gráfico
grafico1 <- data %>% filter(tipodevalor == "Número absoluto") %>%
ggplot(aes(x = anio, y = valor, fill = tipodevalor)) +
  geom_bar(stat = "identity") +
  xlab("Años (2001-2021)") +
  ylab("Número de muertes de mujer") +
  ggtitle("Muertes de mujeres ocasionadas por su pareja y ex-pareja") +
  scale_y_continuous(limits=c(0, max(data$valor))) 

# Segundo gráfico
grafico2 <- data %>% filter(tipodevalor == "Tasa (por cada 100.000 mujeres)") %>%
  ggplot(aes(x = anio, y = valor, fill = tipodevalor))  +
  geom_bar(stat = "identity") +
  xlab("Años (2001-2021)") +
  ylab("Número de muertes de mujer") + 
  ggtitle("Muertes de mujeres ocasionadas por su pareja y ex-pareja") +
  scale_y_continuous(limits=c(0, max(data$valor)))
  

# Mostrar ambos gráficos juntos
gridExtra::grid.arrange(grafico1, grafico2, newpage = TRUE)



