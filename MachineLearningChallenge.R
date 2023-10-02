#Pobranie i wczytanie potrzebnych bibliotek
install.packages(c("caret","e1071", "nnet", "randomforest", "kernlab"))
library(caret)
library(e1071)
library(nnet)
library(randomForest)
library(kernlab)
library(rpart)
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

#################################################################################
#Ten kod trenuje model SVM z jądrem wielomianowym na danych zawartych w zbiorze TrainingData,
#ale bez żadnej krzyżowej walidacji ani strojenia hiperparametrów.
model <- train(Gender ~ .,
               data = TrainingData,
               method = "svmPoly",
               na.action = na.omit, 
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"),
               tuneGrid = data.frame(degree = 1,
                                     scale = 1,
                                     C = 1))

model.cv <- train(Gender ~ .,
                  data = TrainingData,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale", "center"),
                  trControl = trainControl(method = "cv", 
                                           number = 6),
                  tuneGrid = data.frame(degree = 1, 
                                        scale = 1,
                                        C = 1))

#Przedstawienie modeli
print(model)
print(model.cv)
#################################################################################

#Korzystanie z algorytmu randomForest(drzew decyzyjnych)(library(randomForest))
modelr <- randomForest(formula = Gender ~ .,
                       data = Data)
#Przedstawienie modelu
print(modelr)

#################################################################################

#Korzystanie z sieci neuronowych w deeplearningu(library(nnet))
modeln <- nnet(formula = Gender ~ .,
               data = Data,
               size = 30)
#Przedstawienie modelu
print(modeln)
#################################################################################

#Korzystanie z paczki e1071 do stworzenia modelu
modele <- svm(formula = Gender ~ .,
              data = Data)
#Przedstawienie modelu
print(modele)
#################################################################################

#Korzystanie z paczki rpart(dzielenie, klasyfikacja, regresja), wynik to drzewo binarne
partition <- rpart(formula = Gender ~ .,
                   data = Data)
#Wynik będący drzewem binarnym
plot(partition)

#Predykcje na podstawie wczesniejszych modeli
model.test <- predict(model,TestData)
model.cv.test <- predict(model.cv,TestData)
modele.test <- predict(modele, TestData)
modeln.test <- predict(modeln, TestData)
modelr.test <- predict(modelr, TestData)

#Ocena wydajności modelu predykcyjnego przy użyciu macierzy pomyłek
model.train.confusion <- confusionMatrix(model.train, TrainingData$Gender)
model.test.confusion <- confusionMatrix(model.test, TestingData$Gender)
model.cv.confusion <- confusionMatrix(model.cv, TrainingData$Gender)

#Przedstawienie macierzy pomyłek
print(model.train.confusion)
print(model.test.confusion)
print(model.cv.confusion)

#Obliczenie ważności poszczególnych zmiennych przy określaniu wyniku
importance <- varImp(model)
plot(importance, col = "red")



#Wyniki predykcji
print(modele.test)
print(model.test)
print(model.cv.test)
print(modeln.test)
print(modelr.test)

