# Limpiar el entorno
rm(list = ls())

# Establecer el directorio de trabajo
setwd("C:/Users/USUARIO/Documents/Trabajo/BIG Data/Taller#1")

# Cargar el conjunto de datos
data <- read.csv("GEIH2018_filtered_data.csv", stringsAsFactors = FALSE)

# Convertir 'estrato1', 'relab' y 'p6210' a factores
data$estrato1 <- as.factor(data$estrato1)
data$relab <- as.factor(data$relab)
data$p6210 <- as.factor(data$p6210)

# Crear variables dummy para 'estrato1' (1 a 6)
dummies_estrato1 <- model.matrix(~ estrato1 - 1, data = data)  # Incluye todas las categorías
# Renombrar columnas con los niveles reales de estrato1
colnames(dummies_estrato1) <- paste0("estrato1_", levels(data$estrato1))

# Crear variables dummy para 'relab'
dummies_relab <- model.matrix(~ relab - 1, data = data)  # Incluye todas las categorías
# Renombrar columnas con los niveles reales de relab
colnames(dummies_relab) <- paste0("relab_", levels(data$relab))

# Crear variables dummy para 'p6210'
dummies_p6210 <- model.matrix(~ p6210 - 1, data = data)  # Incluye todas las categorías
# Renombrar columnas con los niveles reales de p6210
colnames(dummies_p6210) <- paste0("p6210_", levels(data$p6210))

# Unir las variables dummy al dataframe original, excluyendo las variables categóricas originales
data_dummy <- cbind(data, dummies_estrato1, dummies_relab, dummies_p6210)
data_dummy <- data_dummy[, !(names(data_dummy) %in% c("estrato1", "relab", "p6210"))]

# Modelo 0
modelo0 <- glm(log(y_total_m_ha) ~ age + sex + p6426 + p6870 + p7070 + maxEducLevel + 
                 totalHoursWorked + formal + cuentaPropia + microEmpresa + 
                 estrato1_3 + estrato1_4 + estrato1_5 + estrato1_6 + 
                 p6210_6 + relab_2 + relab_5, data = data_dummy)

# Modelo 7 (Basado en la imagen proporcionada)
modelo7 <- glm(log(y_total_m_ha) ~ age + totalHoursWorked + formal + estrato1_3 + estrato1_4 + 
                 estrato1_5 + estrato1_6 + p6210_6 + relab_2 + relab_5 + p6426 + I(age^2) + 
                 I(totalHoursWorked^2) + age:formal + totalHoursWorked^2:estrato1_3 + I(p6426 * age^2),
               data = data_dummy)

# LOOCV para el Modelo 0
loocv_modelo0 <- cv.glm(data_dummy, modelo0)
loocv_modelo0_error <- loocv_modelo0$delta[1]  # Error de predicción LOOCV para el Modelo 0

# Calcular el RMSE para el Modelo 0
rmse_modelo0 <- sqrt(loocv_modelo0_error)
print(paste("RMSE Modelo 0:", rmse_modelo0))

# LOOCV para el Modelo 7
loocv_modelo7 <- cv.glm(data_dummy, modelo7)
loocv_modelo7_error <- loocv_modelo7$delta[1]  # Error de predicción LOOCV para el Modelo 7

# Calcular el RMSE para el Modelo 7
rmse_modelo7 <- sqrt(loocv_modelo7_error)
print(paste("RMSE Modelo 7:", rmse_modelo7))

