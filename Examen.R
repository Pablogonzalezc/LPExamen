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

