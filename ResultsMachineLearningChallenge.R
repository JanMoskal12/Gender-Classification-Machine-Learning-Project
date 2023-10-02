#Testy modelu opratego na drzewach decyzyjnych zbudowanego na algorytmie randomForest
library(tidyverse)

#Mężczyźni
dane <- Data %>% 
         filter(Gender == "M")
onlymen <- predict(modelr, dane)
print(onlymen)  # Error 2/33

#Kobiety
 dane1 <- Data %>% 
           filter(Gender == "F")
onlywomen <- predict(modelr, dane1)
print(onlywomen) # Error 1/33

#Wszyscy
all <- predict(modelr, Data)
print(all) # Error 3/33




