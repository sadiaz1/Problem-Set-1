# Cargar las bibliotecas necesarias
library(ggplot2)
library(dplyr)
library(stargazer)

data_filtered_select <- data_filtered %>%
  select(age, sex, maxEducLevel, totalHoursWorked, formal, informal, y_total_m, estrato1)

# Crear la tabla descriptiva usando stargazer en formato LaTeX
stargazer(data_filtered_select, type = "latex",
          title = "Descriptive Statistics",
          summary.stat = c("mean", "sd", "min", "max", "n"),
          digits = 2)

# Resumen estadístico de las variables seleccionadas
summary_stats <- data_filtered %>%
  select(age, sex, maxEducLevel, totalHoursWorked, formal, informal, y_total_m, estrato1) %>%
  summary()

# Mostrar la tabla descriptiva
print(summary_stats)

# Gráfico de distribución de edades
ggplot(data_filtered, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# Gráfico de salario total mensual por género
# Transformación logarítmica del salario total mensual
ggplot(data_filtered, aes(x = factor(sex), y = log(y_total_m), fill = factor(sex))) +
  geom_boxplot() +
  labs(title = "Logaritmo del Salario Total Mensual por Género", x = "Género (0 = Mujer, 1 = Hombre)", y = "Log(Salario Total Mensual)") +
  theme_minimal()


# Gráfico de horas trabajadas por nivel educativo
ggplot(data_filtered, aes(x = factor(maxEducLevel), y = totalHoursWorked, fill = factor(maxEducLevel))) +
  geom_boxplot() +
  labs(title = "Horas Trabajadas por Nivel Educativo", x = "Nivel Educativo", y = "Horas Trabajadas") +
  theme_minimal()

# Gráfico de densidad de salario total mensual por estrato socioeconómico
ggplot(data_filtered, aes(x = log(ingtot), fill = factor(estrato1))) +
  geom_density(alpha = 0.6) +
  labs(title = "Densidad del Logaritmo del Salario Total Mensual por Estrato Socioeconómico",
       x = "Log(Salario Total Mensual)", 
       y = "Densidad", 
       fill = "Estrato") +
  theme_minimal()

ggplot(data_filtered, aes(x = log(y_total_m_ha), fill = factor(estrato1))) +
  geom_density(alpha = 0.6) +
  labs(title = "Densidad del Logaritmo del Salario Total Mensual por Estrato Socioeconómico",
       x = "Log(Salario Total Mensual)", 
       y = "Densidad", 
       fill = "Estrato") +
  theme_minimal()


# Gráfico de distribución del tipo de empleo (formal/informal)
ggplot(data_filtered, aes(x = factor(formal), fill = factor(formal))) +
  geom_bar() +
  labs(title = "Distribución del Tipo de Empleo", x = "Tipo de Empleo (0 = Informal, 1 = Formal)", y = "Frecuencia") +
  theme_minimal()
