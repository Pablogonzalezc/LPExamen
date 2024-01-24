install.packages("dplyr")
install.packages("janitor")
install.packages("caret")

library(dplyr)
library(janitor)
library(caret)

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
