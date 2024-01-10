########### Zadanie 1 ########### 
library(ggplot2)
dane <- read.table("earthquake.txt", h = T)
head(dane)

ggplot(data = dane, aes(x = body, y = surface, col = popn)) +
  geom_point(size = 2.5) +   
  theme(axis.title.x = element_text(size = 15, angle = 0, face = "bold"), 
        axis.title.y = element_text(size = 15, angle = 90, face = "bold")) +
  theme(legend.title = element_text(size = 14, face = "bold")) +
  theme(legend.text = element_text(colour = "black", size = 12)) + 
  guides(color = guide_legend(keywidth = 2, keyheight = 2))
# dorzucilem dodatkowe opcje: theme() oraz guides(). nie jest to konieczne, ale
# warto wiedziec, jak modyfikowac wykres. w kazdym razie widzimy liniowa 
# separowalnosc klas

# w modelu musimy dac as.factor(popn), poniewaz jest to kolumna stringow:
typeof(dane$popn)
# wiec trzeba zamienic ja na zmienna kategoryczna

mod <- glm(as.factor(popn) ~ ., data = dane, family = "binomial")
# glm.fit: algorithm did not converge: algorytm nie zbiegl; mamy do czynienia
# z idealna (liniowa) separowalnoscia klas. w takim przypadku sigmoida bedzie 
# zbiegala do funkcji indykatorowej 0-1 ( https://youtu.be/-aCkSfQFdag ),
# czyli bety daza do nieskonczonosci. okazuje sie (efekt Haucka-Donnera), ze 
# w takim wypadku se(\beta) dazy do nieskonczonosci duzo szybciej, niz 
# sama \beta, w zwiazku z czym statystyka walda beta/se(beta) dazy do 0,
# a tym samym p-wartosci daza do 1, czyli zadna zmienna nie jest istotna,
# mimo, ze mamy idealna separowalnosc klas, czyli zmienne idealnie wyjasniaja
# co sie dzieje w danych! rozwiazaniem sa np. metody regularyzacji.

summary(mod)
# ogromne bledy standardowe wspolczynnikow, statystyka Walda zbiega do 0,
# p-wartosci do 1


###################### LAB 11 ###################### 
########### Zadanie 1 ########### 

L = 50 
n = 50
# liczba iteracji
MSE = numeric(length(L)) 
# tutaj skladujemy blad sredniokwadratowy dla kazdej iteracji

for(i in 1:L)
{
  x1 <- rnorm(n, 0, 1)
  x2 <- rnorm(n, 0, 1)
  beta <- c(0.5, 1, 1)
  z <- beta[1] + x1*beta[2] + x2*beta[3]
  p <- 1/(1 + exp(-z))
  y <- rbinom(n, 1, p)
  model <- glm(y ~ x1 + x2, family = "binomial")
  beta_hat <- as.numeric(model$coef)
  MSE[i] <- sum((beta - beta_hat)^2)
}
mean(MSE)

# wazne: rbinom(n, 1, p)
# przypomnijmy: ze rozklad dwumianowy Binom(n, p) z jedna proba (n = 1) czyli
# Binom(1, p) jest tym samym, co rozklad dwupunktowy (Bern(p)). zatem biorac
# np. rbinom(10, 1, 0.3) dostajemy 10 zmiennych losowych z rozkladu dwupunktowego
# z p-stwem sukcesu 0.3. Mozemy tez podac wektor, np. rbinom(2, 1, c(0.1, 0.3)),
# co oznacza, ze losujemy jedna obserwacje z rozkladu dwupunktowego z p = 0.1
# oraz jedna obserwacje z rozkladu dwupunktowego z p = 0.3 (p to p-stwo sukcesu)


N = seq(from = 50, to = 300, by = 10)
MSE = numeric(length(L)) 
meanMSE = numeric(length(N))
j = 1
for(n in N)
{
  for(i in 1:L)
  {
    x1 <- rnorm(n, 0, 1)
    x2 <- rnorm(n, 0, 1)
    beta <- c(0.5, 1, 1)
    z <- beta[1] + x1*beta[2] + x2*beta[3]
    p <- 1/(1 + exp(-z))
    y <- rbinom(n, 1, p)
    model <- glm(y ~ x1 + x2, family = "binomial")
    beta_hat <- as.numeric(model$coef)
    MSE[i] <- sum((beta - beta_hat)^2)
  }
  mean(MSE)
  meanMSE[j] = mean(MSE)
  j = j + 1
}
plot(N, meanMSE, type = "b")
# blad sredniokwadratowy spada wraz ze wzrostem licznosci proby



### Zadanie 2

library(titanic)

### a)
cols <- names(titanic_train)
d <- titanic_train[, -which(cols %in% c("PassengerId", 
                                         "Name", "Ticket", 
                                         "Cabin"))]
### b)
d <- d[which(complete.cases(d)), ] # usuwamy braki
n <- nrow(d)
rownames(d) <- 1:n # usuwajac braki musimy przeindeksowac nasze obserwacje
# inaczej bedziemy mieli dziury w indeksach
### c)

sample_train <- sample(1:n, 0.7*n, replace = F)
sample_test <- setdiff(1:n, sample_train)
dTrain <- d[sample_train, ]
dTest <- d[sample_test, ]

### d)
mod <- glm(Survived ~., data = dTrain, family = "binomial")
probs <- predict(mod, dTest, type = "response")
pred <- ifelse(probs > 0.5, 1, 0)
acc <- length(which(pred == dTest$Survived))/length(pred)

mod_aic <- step(mod, direction = "backward")
probs_aic <- predict(mod_aic, dTest, type = "response")
pred_aic <- ifelse(probs_aic > 0.5, 1, 0)
acc_aic <- length(which(pred_aic == dTest$Survived))/length(pred_aic)

acc; acc_aic
# to jakie dostaniemy wartosci zalezy od wybranego podziali na dane treningowe
# i testowe
 
### PROBLEM: czasem dostajemy komunikat:
# Error in model.frame.default(Terms, newdata, na.action = na.action, 
# xlev = object$xlevels) : czynnik Embarked ma nowe poziomy
# chodzi o to, ze czasem w zbiorze testowym jest wartosc Embarked taka, ktorej nie
# bylo w zbiorze treningowym, w zwiazku z tym model "nie wie", co ma zrobic, bo to
# jest zmienna kategoryczna. przyjrzyjmy sie jakie wartosci ma nasza zmienna Embarked:
table(d$Embarked); unique(d$Embarked)
# mamy dziwna wartosc (dwie obserwacje): "". nic ona nie reprezentuje, wiec 
# jak najbardziej mozemy ja wyrzucic 
d <- d[d$Embarked != "",]
rownames(d) <- 1:nrow(d)

L <- 1000
acc <- numeric(L)
acc_aic <- numeric(L)
for(j in 1:L)
{
  sample_train <- sample(1:n, 0.7*n, replace = F)
  sample_test <- setdiff(1:n, sample_train)
  dTrain <- d[sample_train, ]
  dTest <- d[sample_test, ]
  
  ### d)
  mod <- glm(Survived ~., data = dTrain, family = "binomial")
  probs <- predict(mod, dTest, type = "response")
  pred <- ifelse(probs > 0.5, 1, 0)
  acc[j] <- length(which(pred == dTest$Survived))/length(pred)
  
  mod_aic <- step(mod, direction = "backward")
  probs_aic <- predict(mod_aic, dTest, type = "response")
  pred_aic <- ifelse(probs_aic > 0.5, 1, 0)
  acc_aic[j] <- length(which(pred_aic == dTest$Survived))/length(pred_aic)
}

df = data.frame("MSE" = c(acc, acc_aic),
               "Model" = c(rep("GLM", L), rep("GLM + AIC", L)))

boxplot(MSE ~ Model, data = df)
median(acc_aic); median(acc)
# widzimy, ze mediana jest tylko nieco wyzsza dla selekcji AIC, niz dla 
# pelnego modelu.
