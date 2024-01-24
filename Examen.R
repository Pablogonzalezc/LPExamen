install.packages("dplyr")
install.packages("janitor")
install.packages("caret")
install.packages("xgboost")
install.packages("pROC")

library(dplyr)
library(janitor)
library(caret)
library(xgboost)
library(pROC)

# Carga Datos -------------------------------------------------------------

dataset <- read.csv("dataset_cards.csv")

#kaggle datasets download -d sukuzhanay/credit-card-fraud

# EDA ---------------------------------------------------------------------

count_type <- dataset %>%
  group_by(type, isFraud) %>%
  summarise(count = n())

count_fraud <- dataset %>%
  summarise(total_count = n(), fraud_count = sum(isFraud == 1))

print("Resumen por tipo y isFraud:")
print(count_type)

print("Total de isFraud:")
print(count_fraud)

# LIMPIEZA ----------------------------------------------------------------


dataset$isFraud <- as.factor(dataset$isFraud)

dataset <- dataset %>% janitor::clean_names()

filtered_dataset <- dataset %>% filter(type %in% c("CASH_OUT", "TRANSFER"))

filtered_dataset$is_fraud <- as.factor(filtered_dataset$is_fraud)

undersampled_dataset <- downSample(x = filtered_dataset[, -11], y = filtered_dataset$is_fraud)

dataset_balanced <- data.frame(undersampled_dataset)

summary(dataset_balanced$isFraud)

type_encoding <- c("TRANSFER" = 0, "CASH_OUT" = 1)
dataset_balanced$type_numeric <- as.numeric(factor(dataset_balanced$type, levels = names(type_encoding), labels = type_encoding))

write.csv(dataset_balanced, file = "dataset_cleaned.csv", row.names = FALSE)


# Seleccion y entrenamiento del modelo ------------------------------------

dataset <- read.csv("dataset_cleaned.csv")

# Preparacion datos -------------------------------------------------------

selected_columns <- c("step", "amount", "oldbalance_org", "newbalance_orig", "oldbalance_dest", "newbalance_dest", "is_fraud", "type_numeric")
selected_dataset <- dataset[selected_columns]

set.seed(123)
split_index <- createDataPartition(selected_dataset$is_fraud, p = 0.8, list = FALSE)
train_data <- selected_dataset[split_index, ]
test_data <- selected_dataset[-split_index, ]


matriz_entrenamiento <- xgb.DMatrix(as.matrix(train_data[, -c(7)]), label = train_data$is_fraud)
matriz_prueba <- xgb.DMatrix(as.matrix(test_data[, -c(7)]), label = test_data$is_fraud)



# Modelo XGBOOST ----------------------------------------------------------

parametros <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)

# Entrenamiento -----------------------------------------------------------

modelo_xgboost <- xgboost(data = matriz_entrenamiento, params = parametros, nrounds = 100, verbose = 1)


# Predicciones ------------------------------------------------------------

predicciones <- predict(modelo_xgboost, matriz_prueba)


# Normalizacion de predicciones -------------------------------------------

predicciones_clases <- ifelse(predicciones > 0.5, 1, 0)


# Resultados --------------------------------------------------------------

resultado <- table(predicciones_clases, test_data$is_fraud)
print(resultado)

# Curva ROC ---------------------------------------------------------------

curva_roc <- roc(test_data$is_fraud, predicciones)
plot(curva_roc, col = "blue", main = "Curva ROC")
auc(curva_roc)
