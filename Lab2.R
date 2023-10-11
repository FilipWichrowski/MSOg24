####Zadanie 1 ####

data(hills, package = "MASS")
hills

## a)
### plot(x, y)
par(mfrow = c(1, 2))
plot(hills$dist, hills$time)
plot(hills$climb, hills$time)
# w obu przypadkach przyblizona zaleznosc liniowa

cor(hills$dist, hills$time)
cor(hills$climb, hills$time)
# wartosc wspolczynnika korelacji mierzy sile zaleznosci liniowej
# (tylko wtedy, gdy charakter zaleznosci jest rzeczywiscie liniowy)

## b)
time_dist = lm(time ~ dist, data = hills)
time_climb = lm(time ~ climb, data = hills)

coef(time_dist)[1]; coef(time_dist)[2]

par(mfrow = c(1, 2))
plot(hills$dist, hills$time)
abline(time_dist)
plot(hills$climb, hills$time)
abline(time_climb)
dev.off()

## b) R^2 - wsp. determinacji, 

### z funkcji summary()
summary(time_dist)$r.squared

### z definicji

y = hills$time
SST = sum((y - mean(y))^2) 
SSR = sum((time_dist$fitted.values - mean(y))^2)
# alternatywnie: SSR = sum((fitted(time_dist) - mean(y))^2)
SSE = sum((y - time_dist$fitted.values)^2)
# alterantywnie: SSE = sum(resid(time_dist)^2); resid = y - fitted

SST; SSR + SSE

R2 = SSR/SST
R2

### R^2 to jest kwadrat wsp. korelacji miedzy (y, \hat y)

cor(y, time_dist$fitted.values)^2 

### c)
# funkcja predict()
predict(time_dist, newdata = data.frame(dist = 15))

# za pomoca funkcji coef()
coef(time_dist)[1] + coef(time_dist)[2]*15
### b0 + b1*15

#### Zadanie 2 ####
dane = read.table("anscombe_quartet.txt", header = T)
dane

# podpunkt a) i b)
mod1 = lm(dane$Y1 ~ dane$X1)
mod2 = lm(dane$Y2 ~ dane$X2)
mod3 = lm(dane$Y3 ~ dane$X3)
mod4 = lm(dane$Y4 ~ dane$X4)

par(mfrow = c(2,2))
plot(dane$X1, dane$Y1)
plot(dane$X2, dane$Y2)
plot(dane$X3, dane$Y3)
plot(dane$X4, dane$Y4)

# podpunkt c)
coef(mod1); coef(mod2); coef(mod3); coef(mod3)
summary(mod1)$r.squared; summary(mod2)$r.squared; summary(mod3)$r.squared; summary(mod4)$r.squared

# podpunkt d)

par(mfrow = c(2,2))
plot(dane$X1, dane$Y1)
abline(mod1)
plot(dane$X2, dane$Y2)
abline(mod2)
plot(dane$X3, dane$Y3)
abline(mod3)
plot(dane$X4, dane$Y4)
abline(mod4)

### Zadanie 3
dane = read.table("realest.txt", header = T)
dane

# pdopunkt a)
mod = lm(Price ~ ., data = dane)
X = model.matrix(mod)
X
y = dane$Price
summary(mod)
#### funkcja summary ####
# Residuals:     rozklad rezyduow -- powinnismy miec mediane bliska zera, 
#                rozklad symetryczny (Q3-Q2 \approx Q2-Q1). srednia w modelu
#                z interceptem zawsze rowna 0 
# Estimate:      w MNK kazda beta ma rozklad normalny. "Estimate" to estymatory 
#                kazdej z bet (i tym samym srednie rozkladu). 
# Std. Error:    pierwiastek z wariancji rozkladu estymatora.
# t value:       wartosc statystyki t, sluzacej do testowania dla kazdego 
#                wspolczynnika hipotezy H0, ze dany wspolczynnik jest rowny 0.
# Pr(>|t|):      p-wartosc testu; jesli ponizej poziomu istotnosci, to 
#                mozemy stwierdzic, ze istotnie wspolczynnik jest rozny od zera.
# RSE:           podniesiony do kwadratu daje estymator wariancji naszego bledu,
#                jako ze zakladamy, ze epsilon ~ N(0, \sigma^2)
# R^2:           wspolczynnik determinacji (procent wyjasnionej wariancji)
# Adjusted R^2:  dostosowany R^2, ktory "karze" za liczbe zmiennych w modelu
# F-statistic:   wartosc statystyki F, sluzacej to testowania H0, ze wszystkie
#                wspolczynniki beta sa rowne zero. alternatywna hipoteza to taka,
#                ze przynajmniej jeden z nich jest niezerowy; de facto porownanie
#                naszego modelu z modelem zerowym, w ktorym jest tylko intercept.
#                czyli czy nasz model istotnie poprawia model tylko z interceptem.

# podpunkt b)

solve(t(X) %*% X, t(X) %*% y)
coef(mod)

# zwiekszenie liczby sypialni o 1 powoduje spadek ceny domu o 7.76, ale tylko wtedy
# gdy wartosci wszystkich pozostalych zmiennych sa ustalone.

SST = sum((y - mean(y))^2)
SSR = sum((mod$fitted.values - mean(y))^2)
SSE = sum((mod$fitted.values - y)^2)

SST; SSR + SSE

R2 = SSR/SST
R2
summary(mod)$r.squared

# model tylko z Bedroom: tutaj juz wspolczynnik dodatni

lm(dane$Price ~ dane$Bedroom)

summary(mod)

# podpunkt d)

predict(mod, newdata = data.frame(Bedroom = 3,
                                  Space = 1500,
                                  Room = 8,
                                  Lot = 40,
                                  Tax = 100,
                                  Bathroom = 2,
                                  Garage = 1,
                                  Condition = 0))

# podpunkt e)
# estymator wariancji bledow to \sqrt{SSR}{df}
# df to liczba stopni swobody, czyli n - p - 1 = n - (p + 1), ALE: jesli mamy kolumne 
# z jedynkami, to wtedy mamy p+1 predyktorow i juz nie odejmujemy jedynki, czyli
df = nrow(X) - ncol(X) # bo w macierzy X mamy juz kolumne jedynek
sqrt(SSE/df)^2
summary(mod)$sigma^2
  