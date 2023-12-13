library(MASS) # dla zbioru "longley"
data(longley) # zmienna objasniana to siodma zmienna

########### Zadanie 1 ########### 
### a)

# Macierz korelacji:
X = as.matrix(longley[,-7])
pairs(X)
cors <- round(cor(X), 3) # round(), zeby zaokraglic wynik

# Wspolczynniki determinacji wielokrotnej

R = c()

for(i in 1:ncol(X))
{
  L = lm(X[,i] ~ X[,-i])
  R[i] = summary(L)$r.squared
} 

plot(1:ncol(X), R, ylim = c(0,1))

### Z definicji
Rdef <- c()
for(i in 1:ncol(X))
{
  modFitted <- lm(X[,i] ~ X[,-i])$fitted.values
  covar <- sum((X[,i] - mean(X[,i]))*(modFitted - mean(modFitted)))
  varx <- sum((X[,i] - mean(X[,i]))^2)
  varxhat <- sum((modFitted - mean(modFitted))^2)
  Rdef[i] <- (covar/sqrt((varx * varxhat)))^2
}
all.equal(Rdef, R)

# Wspolczynniki podbicia wariancji vIF:

VIF = c()
for(i in 1:ncol(X))
{
  VIF[i] = 1/(1-R[i])
} 
VIF
car::vif(lm(Employed ~ ., data = longley)) 
# z pakietu car; argumentem jest obiekt modelu


### b)
modL = lm(Employed ~ ., data = longley)
summary(modL)

### c)
library(glmnet)
library(coefplot) # dla funkcji coefpath()
x = as.matrix(longley[,-7])
y = longley[,7]

modL.ridge = glmnet(x, y, alpha = 0, thresh = 10^-14) 
# thresh: threshold for the algorithm to stop converging to the solution
# ogolnie: mamy w glmnet Elastic Net z kara bedaca kombinacja kary L1 oraz L2:
# λ((1 - α)/2*L2 + α*L1)
# zatem alpha = 0 -> ridge
#       alpha = 1 -> LASSO

plot(modL.ridge, xvar = "lambda") # nie mamy podpisanych zmiennych
coefpath(modL.ridge) # tutaj ladniej :))

coef(modL.ridge) # wspolczynniki dla wszystkich rozwazanych w funkcji glmnet lambd
coef(modL.ridge, s = 1) # wspolczynniki dla np. lambdy rownej 1
  
### d)
grid = 10^seq(2, -2, length = 1000) # mozna podac wlasna siatke lambd
# wybieramy lambde kroswalidacyjnie:
modL.ridge.cv = cv.glmnet(x, y, alpha = 0, lambda = grid, nfolds = 5)
plot(modL.ridge.cv) 
# pierwsza linia przerywana z lewej to lambda.min
# druga linia przerywana to lambda.1se: odpowiada ona modelowi, ktory ma MSE 
# o jedno odchylenie standardowe wieksze. ridge moze overfitowac, wiec 
# wybranie wiekszej lambdy (a taka zawsze jest lambda 1.se) odpowiada prostszemu 
# modelowi (wieksze sciaganie wspolczynnikow do zera), ktory tym samym
# bedzie overfitowal zazwyczaj mniej
### PS. jesli nie pojawia Wam sie na osi pionowej Mean-Squared Error, to 
# wywolajcie dev.off()

# wspolczynniki
coef(modL.ridge.cv, s = modL.ridge.cv$lambda.min) 
# albo
betas <- predict(modL.ridge.cv, type = "coef", s = modL.ridge.cv$lambda.min)
betas 
# obie funkcje zwracaja takie same wspolczynniki. mimo, ze glmnet domyslnie standaryzuje
# dane (domyslnym argumentem jest standardize = T), to zwraca wspolczynniki na
# oryginalnej skali predyktorow, a nie wystandaryzowane

# Dodatkowo: predykcja danych
# \hat y z ridge:
predict(modL.ridge.loocv, newx = X[1,], s = modL.ridge.cv$lambda.min)
# \hat y z MNK:
predict(modL, newdata = data.frame(GNP.deflator = 83, GNP = 234.289,
                                   Unemployed = 235.6, Armed.Forces = 159,
                                   Population = 107.608, Year = 1947))
# y rzeczywisty
longley$Employed[1] 

# Problem: w zaleznosci od foldu mamy rozne lambdy. mozna rozwazyc dwa
# podejscia: LOOCV albo kroswalidacje k-krotna powtorzona j-krotnie

### LOOCV:
modL.ridge.loocv <- cv.glmnet(x, y, alpha = 0, lambda = grid, nfolds = nrow(x))
modL.ridge.loocv$lambda.min
coefpath(modL.ridge.loocv)

### KROSWALIDACJA POWTORZONA 100 krotnie
MSEs <- NULL
for (i in 1:100)
{
  cv <- cv.glmnet(x, y, alpha = 0, lambda = grid, nfolds = 3)  
  MSEs <- cbind(MSEs, cv$cvm)
}
rownames(MSEs) <- cv$lambda
lambda.min <- as.numeric(names(which.min(rowMeans(MSEs))))

g <- cv.glmnet(x, y, alpha = 0, lambda = grid, nfolds = 10)
# wazne: dla roznych podzialow mozemy miec rozne siatki lambd zastosowane przez
# algorytm. zeby podejscie z powtorozna kroswalidacja mialo sens, trzeba podac 
# wlasna siatke lambd, zeby w kazdej iteracji byla ona identyczna. 

########### Zadanie 2 ########### 

### a)

prostate = read.table("C:\\Users\\desol\\OneDrive\\Pulpit\\MSO\\Lab8\\prostate.data",h=T)
data = prostate[,-ncol(prostate)]

### b)
model1 = lm(lpsa ~ ., data = data)
model2 = step(model1, direction = "backward", k = 2)

### c)
model = glmnet(as.matrix(data[,-9]), data[,9], alpha = 1) # LASSO
coefpath(model)

### d)
prostate.cv = cv.glmnet(as.matrix(data[,-9]), data[,9], alpha = 1, nfolds = 10)
coefpath(prostate.cv)
plot(prostate.cv)

### e)
predict(prostate.cv, type = "coef", s = prostate.cv$lambda.1se) # dla lambdy.1se
predict(prostate.cv, type = "coef", s = prostate.cv$lambda.min) # dla lambdy.min

########### Zadanie 3 ########### 

p <- 50 # liczba zmiennych
n <- 100 # liczba obserwacji
L <- 100 # liczba powtorzen
beta1_ols = numeric(L)
beta1_ridge = numeric(L)
for(i in 1:L)
  {
   x <- matrix(rnorm(n*p), n, p)
   eps <- rnorm(n)
   beta <- rep(0.1, p)
   y <- x %*% beta + eps
   m1 = lm(y ~ x)
   beta1_ols[i] = coef(m1)[2] # np. bierzemy \beta_2
   
   m2 = glmnet(x, y, alpha = 0)
   beta1_ridge[i] = coef(m2)[2, 50] # wiec tu tez bierzemy \beta_2
   # indeks ,50 oznacza, ze bierzemy to dla piecdziesiatej lambdy,
   # niewazne jaka ona jest
  }
var(beta1_ols)
var(beta1_ridge)

mean(beta1_ols)
mean(beta1_ridge)


