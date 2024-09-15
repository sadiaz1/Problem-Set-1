rm(list=ls())
setwd("C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller#1")
# Cargar los datos desde el archivo CSV
data <- read.csv("GEIH2018_sample_data.csv", stringsAsFactors = FALSE)
# Mostrar los primeros registros del dataset
head(data)
# Mostrar la estructura de los datos
str(data)

library(dplyr)
library(corrplot)
library(ggplot2)
### Filtrar observaciones

data <- data %>% filter(age >= 18)

data <- data %>% filter(!is.na(y_total_m))

# solo los que trabajan
data <- data %>% filter(p6240 == 1)

# Contar el número de NAs en cada columna
na_counts <- sapply(data, function(x) sum(is.na(x)))

# Ordenar las columnas por el número de NAs de mayor a menor
na_counts_sorted <- sort(na_counts, decreasing = TRUE)
print(na_counts_sorted)


###data <- data %>% select(-y_gananciaNetaAgro_m, -p550, -iof3ies, -y_accidentes_m, -cclasnr5)
#### Filtrar Columnas por valores NA, dejamos una completitus del 20%

data <- data %>% select(where(~ sum(is.na(.)) <= 2565))
#20%

selected_vars <- c("directorio", "secuencia_p", "orden", "clase", "dominio", 
                   "age", "sex", "estrato1", "p6210", "p6240", "mes",
                   "relab", "p6426", "p6870", "p6920", "p7040", "p7070", "y_total_m_ha", 
                   "maxEducLevel", "totalHoursWorked", "formal", "informal", 
                   "cuentaPropia", "microEmpresa", "sizeFirm", "y_total_m", 
                   "college", "depto", "ingtot")
#relab es p6430
#filtramos variables
data_filtered <- data %>% select(all_of(selected_vars))

data_filtered <- data_filtered %>%
  mutate(maxEducLevel = ifelse(is.na(maxEducLevel) == TRUE, 1 , maxEducLevel))
# lo ponemos en millones
data_filtered <- data_filtered  %>%
  mutate(ingtot =ingtot/1000000 )

data_filtered <- data_filtered %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

write.csv(data_filtered, "GEIH2018_filtered_data.csv", row.names = FALSE)

summary(data$relab)

#posibles variables a añadir
#p6750
#p6760 


data_numeric <- data_filtered %>% 
  mutate(across(where(is.character), as.numeric)) %>%
  select(where(is.numeric))




data2 <- read.csv("GEIH2018_filtered_data.csv", stringsAsFactors = FALSE)


# Verificar que no queden NA en las columnas numéricas
sum(is.na(data2))
data_filtered <- data2




