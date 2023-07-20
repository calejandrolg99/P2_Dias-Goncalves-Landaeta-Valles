#LIBRERIAS----------------------------------------------------------------------

library(readxl) # Leer archivos de excel
library(tidyverse) # Manejo de base de datos y gráficos
library(tidyr) 
library(cowplot) # Permite organizar fácilmente varios gráficos en una sola figura
library(xlsx) # Leer, escribir y formatear archivos de Excel
library(countrycode)
library(rmarkdown)
library(ggplot2)

#CARGA DE DATOS-----------------------------------------------------------------

data <- read_excel("BaseDeDatosOriginal.xlsx")
data1 <- read_excel("TotalMuejeres.xlsx")
data2 <- read_excel("ProporcionQueNoDenuncian.xlsx")
data3 <- read_excel("Legislaciones.xlsx")
data4 <- read_excel("MuertesMujeres.xlsx")
data5 <- read_excel("LegislacionAbusoDomestico.xlsx")

#ORDENAR Y LIMPIAR TABLAS-------------------------------------------------------

# Aplica a la base de datos "data"
# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data <- data %>% arrange(País__ESTANDAR, Años__ESTANDAR) %>% filter(Años__ESTANDAR >= 2000 & Años__ESTANDAR <= 2020) %>% select(País__ESTANDAR, Años__ESTANDAR, value, Tipodevalor) %>% rename(Value = value, Entity = País__ESTANDAR, Year = Años__ESTANDAR)

#Lista de Paises
country_list <- unique(data$Entity)

# Aplica a la base de datos "data1"
# Código para ordenar por paises y por año. Además para filtrar los no continentes y por los años de interés
data1 <- data1 %>% arrange(Entity, Year) %>% 
  filter(Entity != "Channel Islands" & Entity != "East Asia and Pacific" &
           Entity != "Europe and Central Asia" & Entity != " European Union" & 
           Entity != "World" & Entity !="High income" & 
           Entity != "Latin America and Caribbean" & Entity != "Low and middle income"  & 
           Entity != "Lower middle income" & Entity !="High income" &
           Entity != "Micronesia (country)" &  Entity!="Middle East and North Africa" & 
           Entity != "Middle income" &  Entity != "North America" & 
           Entity != "South Asia" &  Entity!= "Sub-Saharan Africa"& Entity  !="Upper middle income"  & 
           Entity != "Aruba" & Entity != "Curacao" & Entity != "European Union" & 
           Entity != "French Polynesia" & Entity != "Guam" & Entity != "Hong Kong" & 
           Entity != "Low income" & Entity != "Macao" &
           Entity != "New Caledonia"  & Entity != "Palestine" & 
           Entity != "United States Virgin Islands" & Entity != "Low income" & Entity != "Puerto Rico", 
         Year >= 2000 & Year <= 2020)

# Añade una nueva columna con los países en español
data1$Entity_es <- countrycode(data1$Entity, "country.name.en", "un.name.es")

# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data1 <- data1 %>% select(Entity_es, Code, Year, Population) %>% rename(Entity = Entity_es)

# Aplica a la base de datos "data2"
# Código para ordenar por paises y por año. Además para filtrar los no continentes y por los años de interés
data2 <- data2 %>% arrange(CountryName, Year) %>% 
  filter(CountryName != "East Asia & Pacific" & CountryName != "Europe & Central Asia" & 
           CountryName != "Hong Kong SAR, China" &   CountryName != "High income" &
           CountryName != "Latin America & Caribbean" & 
           CountryName != "Low income"  & CountryName !="Lower middle income" &
           CountryName != "Middle East & North Africa" & CountryName != "North America" & 
           CountryName != "South Asia" &  CountryName!= "Sub-Saharan Africa" &
           CountryName  !="Upper middle income" & 
           CountryName != "West Bank and Gaza" & CountryName != "Kosovo", Year >= 2000 & Year <= 2020)

# Añade una nueva columna con los países en español
data2$Entity_es <- countrycode(data2$CountryName, "country.name.en", "un.name.es")

# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data2 <- data2 %>% select(Entity_es, CountryCode, Year, Value) %>% rename(Entity = Entity_es, Code = CountryCode, NotReport = Value)

# Aplica a la base de datos "data3"
# Añade una nueva columna con los países en español
data3$Entity_es <- countrycode(data3$CountryName, "country.name.en", "un.name.es") 

# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data3 <- data3 %>% arrange(Entity_es, Year) %>% select(Entity_es, CountryCode, Year, Value) %>% rename(Entity = Entity_es, Code = CountryCode, Legislation= Value)

# Aplica a la base de datos "data4"
# Código para ordenar por paises y por año. Además para filtrar los no continentes y por los años de interés
data4 <- data4 %>% arrange(CountryName, Year) %>% 
  filter(CountryName != "Africa Eastern and Southern" & CountryName != "Africa Western and Central" & 
           CountryName != "American Samoa" &   CountryName != "Arab World" &
           CountryName != "Aruba" & CountryName != "Bermuda"  & CountryName !="Caribbean small states" &
           CountryName != "Cayman Islands" & CountryName != "Central Europe and the Baltics" & 
           CountryName != "EAR" &  CountryName!= "East Asia & Pacific (IDA & IBRD)" &
           CountryName != "Euro area" & CountryName != "Europe & Central Asia (excluding high income)" &
           CountryName != "Europe & Central Asia"& CountryName != "Europe & Central Asia (IDA & IBRD)" & 
           CountryName != "European Union" & CountryName != "Fragile and conflict affected situations" &   
           CountryName != "French Polynesia" & CountryName != "Gibraltar" & 
           CountryName != "Greenland"  & CountryName !="Guam" &
           CountryName != "Heavily indebted poor countries (HIPC)" & CountryName != "High income" & 
           CountryName != "Curacao" &  CountryName!= "East Asia & Pacific" & 
           CountryName  !="East Asia & Pacific (excluding high income)" & 
           CountryName != "British Virgin Islands" & CountryName != "Channel Islands" & 
           CountryName != "Hong Kong SAR, China" & CountryName != "IBRD only " &
           CountryName != "IDA & IBRD total" & CountryName != "IDA blend" & 
           CountryName != "IDA only" & CountryName != "Isle of Man" & CountryName != "Kosovo" & 
           CountryName != "Latin America & Caribbean" & 
           CountryName != "Latin America & Caribbean (excluding high income)" & 
           CountryName != "Latin America & Caribbean (IDA & IBRD)" & 
           CountryName != "Least developed countries: UN classification" & 
           CountryName != "LMY" & CountryName != "Low income" & CountryName != "Lower middle income" & 
           CountryName != "LTE" &  CountryName != "Macao SAR, China" & CountryName != "IBRD only" & 
           CountryName != "MIC" &  CountryName != "Middle East & North Africa"  &  
           CountryName != "IDA total" & CountryName != "Middle East & North Africa (excluding high income)" &  
           CountryName != "Middle East & North Africa (IDA & IBRD)"  &  CountryName != "OECD members" &
           CountryName != "Other small states" &  CountryName != "Pacific island small states"  &  
           CountryName != "PRE" & CountryName != "PST" &  CountryName != "Small states"  &  
           CountryName != "South Asia (IDA & IBRD)" & CountryName != "St. Martin (French part)" &  
           CountryName != "Sub-Saharan Africa (excluding high income)"  &  
           CountryName != "Sub-Saharan Africa (IDA & IBRD)" &
           CountryName != "Turks and Caicos Islands" &  CountryName != "West Bank and Gaza" & CountryName != "World" & 
           CountryName != "North America" & CountryName != "South Asia" & CountryName != "Sub-Saharan Africa" &
           CountryName != "Upper middle income" & CountryName != "Puerto Rico",
         Year >= 2000 & Year <= 2020)


# Añade una nueva columna con los países en español
data4$Entity_es <- countrycode(data4$CountryName, "country.name.en", "un.name.es")

# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data4 <- data4 %>% select(Entity_es, CountryCode, Year, Value) %>% rename(Entity = Entity_es, Code = CountryCode, Death = Value)

# Aplica a la base de datos "data5"
# Código para ordenar por paises y por año.
data5 <- data5 %>%
  pivot_longer(cols = `2000 [YR2000]`:`2020 [YR2020]`, names_to = "Year", values_to = "Value") %>%
  select(`Country Name`, `Country Code`, Year, Value) %>%
  rename(Entity = `Country Name`, Code = `Country Code`, HasLegislationDomesticViolence = Value)

#Formato de años
data5$Year <- as.numeric(gsub("\\[.*\\]", "", data5$Year))

#Traduccion de nombres de paises
data5$Entity_es <- countrycode(data5$Entity, "country.name.en", "un.name.es") 
data5 <- data5 %>%
  select(Entity_es, Code, Year, HasLegislationDomesticViolence) %>%
  rename(Entity = Entity_es)

#FILTRAR PARA OBTENER PAISES ESTABLECIDOS --------------------------------------

data1 <- data1 %>%
  filter(Entity %in% country_list)

data2 <- data2 %>%
  filter(Entity %in% country_list)

data3 <- data3 %>%
  filter(Entity %in% country_list)

data4 <- data4 %>%
  filter(Entity %in% country_list)

data5 <- data5 %>%
  filter(Entity %in% country_list)
