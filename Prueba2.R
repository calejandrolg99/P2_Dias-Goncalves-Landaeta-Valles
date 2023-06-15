
install.packages("cowplot")
install.packages("xlsx")

#librería
library(readxl)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(xlsx)

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

#Colores para los graficos de linea
colors_vec <- c("#4d4dff", "#00b3b3", "#00cc99", "#b3b300", "#b35900", "#cc99cc", "#ff4d4d", "#808080", "#ff9933", "#6666ff", "#00e6e6", "#b3b3b3", "#ff66ff", "#ffcc99", "#99ff99", "#ff6666", "#b3b300", "#00b3b3", "#ff99cc", "#6666ff", "#ffcc00", "#cc00cc", "#ff4d4d", "#00cc00", "#b35900", "#4d4dff", "#808080", "#FF9933", "#99E6E6", "#FF99CC", "#6666FF", "#FFCC70", "#CC00CC")
names(colors_vec) <- c("Costa Rica", "Trinidad y Tabago", "San Vicente y las Granadinas", "Colombia", "España", "Granada", "Jamaica", "Nicaragua", "Puerto Rico", "República Dominicana", "Santa Lucía", "Suriname", "Dominica", "Saint Kitts y Nevis", "Islas Vírgenes Británicas", "Anguila", "Antigua y Barbuda", "Guyana", "Montserrat", "Chile", "Paraguay", "Uruguay", "Perú", "Honduras", "Barbados", "Belice", "El Salvador", "Argentina", "Venezuela (República Bolivariana de)", "Ecuador", "Portugal", "Cuba", "Panamá")

#Grafico de linea con el valor absoluto
line_absoluto = data %>%
  filter(tipodevalor == 'Número absoluto') %>%
  ggplot(aes(x = anio, y = valor, color = paises)) +
  geom_line(aes(group = paises)) +
  scale_color_manual(values = colors_vec) +
  labs(title = "Tendencia en numero de muertes de mujeres ocasionadas por parejas y  ex-parejas",
       x = "Año",
       y = "Numero de muertes") 

#Grafico de linea con la tasa de muertes, por cada 100000 mujeres
line_tasa = data %>%
  filter(tipodevalor == 'Tasa (por cada 100.000 mujeres)') %>%
  ggplot(aes(x = anio, y = valor, color = paises)) +
  geom_line(aes(group = paises)) +
  scale_color_manual(values = colors_vec) +
  labs(title = "Tendencia en la tasa de muertes de mujeres ocasionadas por parejas y  ex-parejas",
       x = "Año",
       y = "Tasa (por cada 100.000 mujeres)") 

#Mostrar ambos graficos juntos
gridExtra::grid.arrange(line_absoluto, line_tasa, newpage = TRUE)

#Para poner en un excel la data limpia
xlsx::write.xlsx(data, file = "BaseDeDatosLimpia.xlsx")
