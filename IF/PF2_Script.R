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
               
continentes <- tribble(
  ~pais,                    ~continente,
  "Costa Rica",               "Central America",
  "Trinidad y Tabago",      "South America",
  "San Vicente y las Granadinas", "South America",
  
  "Colombia", "South America",
  "España",  "Europe",
  "Granada",  "North America",
  
  "Jamaica", "North America",
  "Nicaragua", "North America",
  "Puerto Rico", "North America",
  
  "República Dominicana", "North America",
  "Santa Lucía",  "North America",
  "Suriname", "South America",
  
  "Dominica", "North America",
  "Saint Kitts y Nevis", "North America",
  "Islas Vírgenes Británicas", "North America",
  
  "Anguila", "North America",
  "Antigua y Barbuda",  "North America",
  "Guyana", "South America",
  
  "Montserrat", "North America",
  "Chile",  "South America",
  "Paraguay", "South America",
  
  "Uruguay", "South America",
  "Perú",  "South America",
  "Honduras", "North America",
  
  "Barbados", "North America",
  "Belice", "North America",
  "El Salvador", "North America",
  
  "Argentina", "South America",
  "Venezuela (República Bolivariana de)", "South America",
  "Ecuador", "South America",
  "Portugal", "Europe",
  "Cuba",  "North America", 
  "Panamá", "Central America")
  
# asignar continentes a la base de datos data
data <- data %>% mutate(continente = ifelse(Entity %in% continentes$pais, 
                                              yes= continentes$continente[match(Entity, continentes$pais)], 
                                              no= NA))

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
           CountryName != "Upper middle income" & CountryName != "Puerto Rico" & CountryName != "New Caledonia" & CountryName != "Virgin Islands (U.S.)",
         Year >= 2000 & Year <= 2020, `Indicator Name` == "Intentional homicides, female (per 100,000 female)")


# Añade una nueva columna con los países en español
data4$Entity_es <- countrycode(data4$CountryName, "country.name.en", "un.name.es")

# Seleccionamos las columnas de interés y cambiamos el nombre a las columnas
data4 <- data4 %>% select(Entity_es, CountryCode, Year, Value) %>% rename(Entity = Entity_es, Code = CountryCode, Death = Value)

# Aplica a la base de datos "data5"
# Código para ordenar por paises y por año.
data5 <- data5 %>%
  pivot_longer(cols = `2000 [YR2000]`:`2020 [YR2020]`, names_to = "Year", values_to = "Value") %>%
  select(`Country Name`, `Country Code`, Year, Value) %>%
  rename(Entity = `Country Name`, Code = `Country Code`, HasLegislationDomesticViolence = Value) %>%
  arrange(Entity, Year) %>% 
  filter(Entity != "Africa Eastern and Southern" & Entity != "Africa Western and Central" & 
           Entity != "American Samoa" &   Entity != "Arab World" &
           Entity != "Aruba" & Entity != "Bermuda"  & Entity !="Caribbean small states" &
           Entity != "Cayman Islands" & Entity != "Central Europe and the Baltics" & 
           Entity != "EAR" &  Entity!= "East Asia & Pacific (IDA & IBRD)" &
           Entity != "Euro area" & Entity != "Europe & Central Asia (excluding high income)" &
           Entity != "Europe & Central Asia"& Entity != "Europe & Central Asia (IDA & IBRD)" & 
           Entity != "European Union" & Entity != "Fragile and conflict affected situations" &   
           Entity != "French Polynesia" & Entity != "Gibraltar" & 
           Entity != "Greenland"  & Entity !="Guam" &
           Entity != "Heavily indebted poor countries (HIPC)" & Entity != "High income" & 
           Entity != "Curacao" &  Entity!= "East Asia & Pacific" & 
           Entity  !="East Asia & Pacific (excluding high income)" & 
           Entity != "British Virgin Islands" & Entity != "Channel Islands" & 
           Entity != "Hong Kong SAR, China" & Entity != "IBRD only " &
           Entity != "IDA & IBRD total" & Entity != "IDA blend" & 
           Entity != "IDA only" & Entity != "Isle of Man" & Entity != "Kosovo" & 
           Entity != "Latin America & Caribbean" & 
           Entity != "Latin America & Caribbean (excluding high income)" & 
           Entity != "Latin America & Caribbean (IDA & IBRD)" & 
           Entity != "Least developed countries: UN classification" & 
           Entity != "LMY" & Entity != "Low income" & Entity != "Lower middle income" & 
           Entity != "LTE" &  Entity != "Macao SAR, China" & Entity != "IBRD only" & 
           Entity != "MIC" &  Entity != "Middle East & North Africa"  &  
           Entity != "IDA total" & Entity != "Middle East & North Africa (excluding high income)" &  
           Entity != "Middle East & North Africa (IDA & IBRD)"  &  Entity != "OECD members" &
           Entity != "Other small states" &  Entity != "Pacific island small states"  &  
           Entity != "PRE" & Entity != "PST" &  Entity != "Small states"  &  
           Entity != "South Asia (IDA & IBRD)" & Entity != "St. Martin (French part)" &  
           Entity != "Sub-Saharan Africa (excluding high income)"  &  
           Entity != "Sub-Saharan Africa (IDA & IBRD)" &
           Entity != "Turks and Caicos Islands" &  Entity != "West Bank and Gaza" & Entity != "World" & 
           Entity != "North America" & Entity != "South Asia" & Entity != "Sub-Saharan Africa" &
           Entity != "Upper middle income" & Entity != "Puerto Rico" &
           Entity != "Early-demographic dividend" & Entity != "Faroe Islands" & 
           Entity != "Late-demographic dividend" & Entity != "Low & middle income" & 
           Entity != "Middle income" & Entity != "New Caledonia"& 
           Entity != "Northern Mariana Islands" & Entity != "Post-demographic dividend" & 
           Entity != "Pre-demographic dividend" & Entity != "Sint Maarten (Dutch part)" & 
           Entity != "Virgin Islands (U.S.)")

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

#GRAFICOS ----------------------------------------------------------------------
#Utilizando la base de datos data
colors_vec <- c("#4d4dff", "#00b3b3", "#00cc99", "#b3b300", "#b35900", "#cc99cc", "#ff4d4d", "#808080", "#ff9933", "#6666ff", "#00e6e6", "#b3b3b3", "#ff66ff", "#ffcc99", "#99ff99", "#ff6666", "#b3b300", "#00b3b3", "#ff99cc", "#6666ff", "#ffcc00", "#cc00cc", "#ff4d4d", "#00cc00", "#b35900", "#4d4dff", "#808080", "#FF9933", "#99E6E6", "#FF99CC", "#6666FF", "#FFCC70", "#CC00CC")
names(colors_vec) <- c("Costa Rica", "Trinidad y Tabago", "San Vicente y las Granadinas", "Colombia", "España", "Granada", "Jamaica", "Nicaragua", "Puerto Rico", "República Dominicana", "Santa Lucía", "Suriname", "Dominica", "Saint Kitts y Nevis", "Islas Vírgenes Británicas", "Anguila", "Antigua y Barbuda", "Guyana", "Montserrat", "Chile", "Paraguay", "Uruguay", "Perú", "Honduras", "Barbados", "Belice", "El Salvador", "Argentina", "Venezuela (República Bolivariana de)", "Ecuador", "Portugal", "Cuba", "Panamá")

grafico1 = data %>%
  filter(Tipodevalor == 'Tasa (por cada 100.000 mujeres)') %>% 
  ggplot(aes( x= Year, y= Value, color=continente)) + 
  geom_line() +  
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Comparacion de las tasas por cada 100.000 mujeres", "Separados por pais") + 
  facet_wrap(~continente) 

grafico1


new_data <- left_join(data,data5,by=c("Entity","Year"))
women_grouped <- new_data %>% 
  filter(Tipodevalor == "Tasa (por cada 100.000 mujeres)") %>%
  group_by(Entity, HasLegislationDomesticViolence) %>% 
  summarize(mean_death_rate = mean(Value))

grafico2 <- ggplot(women_grouped, aes(x = Entity, y = mean_death_rate, fill = HasLegislationDomesticViolence)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparacion de Tasas de muerte por status de legislacion",
       x = "Pais",
       y = "Tasa de Muerte promedio",
       fill = "Status de legislacion") +
  theme(plot.title = element_text(hjust = 0.5))

grafico2

grafico3 = data %>%
  filter(Tipodevalor == 'Número absoluto') %>% 
  ggplot(aes( x= Year, y= Value, color=continente)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 45)) + 
  ggtitle("Comparacion de las muertes de Mujeres", "Separados por pais") + 
  facet_wrap(~continente) 

grafico3



