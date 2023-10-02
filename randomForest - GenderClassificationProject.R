# Wczytanie potrzebnych bibliotek
library(caret)
library(randomForest)

#Ustawienie ziarna generatora, po to żeby później porónać wyniki
set.seed(10)

# Wczytywanie danych z pliku csv oraz obejrzenie ich
Data <- read.csv("GenderClassification.csv", stringsAsFactors = TRUE)
View(Data)
str(Data)

#Zamiana typu zmiennych z factor na numeric(cleaning data), ponieważ ten typ jest wygodniejszy
Data$Favorite.Color <- as.numeric(Data$Favorite.Color)
Data$Favorite.Music.Genre <- as.numeric(Data$Favorite.Music.Genre)
Data$Favorite.Beverage <- as.numeric(Data$Favorite.Beverage)
Data$Favorite.Soft.Drink <- as.numeric(Data$Favorite.Soft.Drink)

#Skorzystanie z createDataPartion(library(caret)), żeby podzielić dane na training data i test data
TrainingDataSize <- createDataPartition(Data$Gender,
                                        p = 0.8,
                                        list = FALSE)
TrainingData <- Data[TrainingDataSize,]
TestData <- Data[-TrainingDataSize,]

#Korzystanie z algorytmu randomForest(drzew decyzyjnych)(library(randomForest))
modelr <- randomForest(formula = Gender ~ .,
                       data = Data)
#Przedstawienie modelu
print(modelr)

#Testy
modelr1.test <- predict(modelr, TestData)
TestData
print(modelr1.test)
modelr1.test1 <- predict(modelr, Data)
print(modelr1.test1)

