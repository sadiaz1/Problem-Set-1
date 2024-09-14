# Limpiar el entorno
rm(list = ls())

# Establecer el directorio de trabajo
setwd("C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller#1")

# Cargar los datos desde el archivo CSV
data <- read.csv("GEIH2018_filtered_data.csv", stringsAsFactors = FALSE)

# Instalar y cargar el paquete boot
install.packages("boot")
library(boot)

# Modelo 1
modelo1 <- glm(log(y_total_m_ha) ~ age + sex + p6426 + p6870 + p7040 + p7070 + totalHoursWorked + 
                 cuentaPropia + microEmpresa + estrato1 + relab + orden + I(p7070^2) + 
                 I(maxEducLevel^2) + I(formal^2) + I(college^2), data = data)

# Modelo 2
modelo2 <- glm(log(y_total_m_ha) ~ age + totalHoursWorked + formal + estrato1 + p6426 + I(age^2) + 
                 I(totalHoursWorked^2) + age:formal + I(totalHoursWorked^2):estrato1 + p6426:I(age^2), data = data)

# LOOCV para modelo 1
loocv_modelo1 <- cv.glm(data, modelo1)
loocv_modelo1_error <- loocv_modelo1$delta[1]  # Error de predicción LOOCV para el modelo 1

# LOOCV para modelo 2
loocv_modelo2 <- cv.glm(data, modelo2)
loocv_modelo2_error <- loocv_modelo2$delta[1]  # Error de predicción LOOCV para el modelo 2

# MSE para modelo 1
y_real <- data$y_total_m_ha  # Utiliza la variable presente en tu dataframe
y_predicho1 <- predict(modelo1, newdata = data, type = "link")
mse_modelo1 <- mean((log(y_real) - y_predicho1)^2)

# MSE para modelo 2
y_predicho2 <- predict(modelo2, newdata = data, type = "link")
mse_modelo2 <- mean((log(y_real) - y_predicho2)^2)

# Calculo RMSE
rmse_modelo1 <- sqrt(loocv_modelo1_error)
print(rmse_modelo1)

rmse_modelo2 <- sqrt(loocv_modelo2_error)
print(rmse_modelo1)

