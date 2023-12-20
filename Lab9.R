library(MASS)
library(pls)

########### Zadanie 1 ########### 

data(Boston)
n <- nrow(Boston)
set.seed(1)

### a)
train_idx <- sample(1:n, 0.7*n, replace = F)
test_idx <- setdiff(1:n, train_idx)

TrainB <- Boston[train_idx,]
TestB <- Boston[test_idx,]

### b)
# PCR:
model_pcr <- pcr(medv ~., data = TrainB, scale = T, validation = "CV")
summary(model_pcr)
validationplot(model_pcr, val.type = "RMSEP") # optymalne wydaje sie 6 skladowych
validationplot(model_pcr, val.type = "R2") # j.w.
plot(pcr_pred ~ TestB$medv) # porownujemy predykcje oraz rzeczywistego y:
# im bardziej punkty ukladaja sie w zaleznosc liniowa, tym lepsza mamy predykcje
pcr_pred <- predict(model_pcr, TestB, ncomp = 6)
# RMSEP
rmsep_pcr <- sqrt(mean((pcr_pred - TestB$medv)^2))

# PLSR:
mod_plsr <- plsr(medv ~., data = TrainB, scale = T, validation = "CV")
summary(mod_plsr)
validationplot(mod_plsr, val.type = "MSEP") # podobnie jak przy PCR: optymalne
# wydaje sie 6 skladowych glownych
validationplot(mod_plsr, val.type = "R2")

pred_plsr <- predict(model_plsr, TestB, ncomp = 6)
# RMSEP
rmsep_plsr <- sqrt(mean((pred_plsr - TestB$medv)^2))

rmsep_plsr; rmsep_pcr # mamy mniejszy blad sredniokwadratowy dla PLSR,
# co jest oczekwiane: w koncu PLSR, w przeciwienstwie do PCR, bierze pod
# uwage zmienna odpowiedzi


### Zadanie 2
mse1pcr <- numeric(); mse3pcr <- numeric(); mse6pcr <- numeric()
mse1plsr <- numeric(); mse3plsr <- numeric(); mse6plsr <- numeric()
for(i in 1:50)
{
  set.seed(i)
  train_idx <- sample(1:n, 0.7*n, replace = F)
  test_idx <- setdiff(1:n, train_idx)
  
  TrainB <- Boston[train_idx,]
  TestB <- Boston[test_idx,]
  model_pcr <- pcr(medv ~., data = TrainB, scale = T, validation = "CV")
  model_plsr <- plsr(medv ~., data = TrainB, scale = T, validation = "CV")
  mse1pcr[i] <- sqrt(mean((predict(model_pcr, TestB, ncomp = 1) - TestB$medv)^2))
  mse3pcr[i] <- sqrt(mean((predict(model_pcr, TestB, ncomp = 3) - TestB$medv)^2))
  mse6pcr[i] <- sqrt(mean((predict(model_pcr, TestB, ncomp = 6) - TestB$medv)^2))
  mse1plsr[i] <- sqrt(mean((predict(model_plsr, TestB, ncomp = 1) - TestB$medv)^2))
  mse3plsr[i] <- sqrt(mean((predict(model_plsr, TestB, ncomp = 3) - TestB$medv)^2))
  mse6plsr[i] <- sqrt(mean((predict(model_plsr, TestB, ncomp = 6) - TestB$medv)^2))
}

d = data.frame("RMSE" = c(mse1pcr, mse3pcr, mse6pcr, mse1plsr, mse3plsr, mse6plsr),
               "Model" = c(rep("PCR", 150), rep("PLSR", 150)),
           "PCs" = rep(c(rep("1PC", 50), rep("3PC", 50), rep("6PC", 50)), 2))
library(ggplot2)

boxplot(RMSE ~ Model + PCs, data = d)
# co widzimy?
# 1) im wiecej skladowych glownych w PCR i PLSR,
#    tym mniejszy blad sredniokwadratowy
# 2) dla wybranej liczby skladowych glownych (1,3,6), mediana RMSE dla PLSR
#    jest nizsza, niz mediana dla PCR -- tego bysmy sie spodziewali. rozstepy
#    miedzykwartylowe sa niemal identyczne dla kazdego z boxplotow, zatem
#    zmiennosc RMSE wydaje sie byc podobna dla kazdej metody i kazdej liczby PC

