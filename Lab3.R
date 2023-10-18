### Zad 1 ### 

dane  = read.table("airpollution.txt",h=TRUE)
dane = dane[,-ncol(dane)] # usuwamy zdublowana kolumne

# a)
model = lm(Mortality ~ ., data = dane) 
summary(model)

# b)
# statystyka t = beta/se(beta)

summary(model)$coef[14,3] # to stat. t z modelu
summary(model)$coef[14,4] # a to jej p-wartosc

# liczymy recznie statystyke 

y <- dane$Mortality
X <- model.matrix(model) # dodatkowa kolumna jedynek!
n = nrow(X)
p = ncol(X) # uwzgledniamy w tym intercept!
QR <- qr(X)
R <- qr.R(QR)
Q <- qr.Q(QR)

# beta
beta <- backsolve(R, t(Q) %*% y)

# se(beta)
xtx_inv <- solve(R)%*%t(solve(R))

SSE <- sum(residuals(model)^2)
sigma2 <- SSE/(n - p)
sqrt(sigma2) == summary(model)$sigma # zgadza sie

se_beta <- sqrt(sigma2 * diag(xtx_inv))
all.equal(se_beta, summary(model)$coef[,2]) # zgadza sie

tstat <- beta/se_beta
tstat
summary(model)$coef[,3] # tez jest ok


# p-wartosc: P(T > |t|)
2 - 2*pt(summary(model)$coef[14, 3], n - p) # p-wartosc dla zmiennej NOxPot
2*pt(-summary(model)$coef[14, 3], n - p) # inaczej


# c)
SSR <- sum((predict(model) - mean(y))^2)
SSE <- sum((predict(model) - y)^2)
f = (SSR/(p-1))/(SSE/(n-p))
f
summary(model)$fstatistic  # statystyka F z summary()

1 - pf(f, p -1, n - p) # p-wartosc statystyki F

### Zad 2 ### 

# generowanie danych
n = 1000
p <- 2
x = runif(n)
e = rnorm(n, 0, 1)
y = 0.5 + 1*x^2 + e
model = lm(y~x)
sse <- sum((predict(model) - y)^2)
summary(model)

# zmienna x- istotna wg. testu t 
# Wniosek: Zmienna moze byc istotna wg testu,
# nawet jezeli specyfikacja modelu jest zla! 
# oczywiscie mozna to zadanie robic dokladnie tak,
# jak robilismy poprzednie (recznie: z QR, etc.)


### Zad 3 ### 

# a) obliczanie mocy testu dla ustalonego n

n = 100
B = 100
moc1 = numeric(B)
moc2 = numeric(B)
moc3 = numeric(B)

for(b in 1:B)
{
  # generowanie danych:
  x1 = rnorm(n, 0, 1)
  x2 = rnorm(n, 0, 1)
  x3 = rnorm(n, 0, 1)
  e = rnorm(n, 0, 1)
  y = 0.5 + 1*x1 + 0.5*x2 + 0.05*x3 + e
  model = lm(y ~ x1 + x2 + x3)
  moc1[b] = ifelse(summary(model)$coef[2,4] < 0.05, 1, 0)
  moc2[b] = ifelse(summary(model)$coef[3,4] < 0.05, 1, 0)
  moc3[b] = ifelse(summary(model)$coef[4,4] < 0.05, 1, 0)
}


MOC1 = mean(moc1)
MOC2 = mean(moc2)
MOC3 = mean(moc3)

MOC1; MOC2; MOC3

# Wnioski:
# - Moc zalezy od wielkosci prawdziwego wspolczynnika
# - Moc zalezy od n

# Wyjasnienie do tego zadania: moc testu to p-stwo odrzucenia 
# hipotezy zerowej, gdy jest falszywa. w poleceniu jest test t,
# zatem przypomnijmy: hipoteza zerowa w tescie t mowi, ze dla 
# i-tej zmiennej x_i mamy beta_i = 0.
# my natomiast widzimy, ze w naszym modelu PRAWDZIWE wartosci
# wspolczynnikow to beta0 = 0.5, beta1 = 1, beta2 = 0.5, beta3 = 0.05. 
# co to znaczy? to znaczy, ze dla kazdej zmiennej x_i hipoteza 
# zerowa, ze beta_i = 0, jest falszywa! 
# dlaczego tego potrzebujemy?
# dlatego, ze rownowazna definicja p-wartosci jest taka, ze to p-stwo, 
# ze POPRAWNIE otrzymamy wartosc p < \alpha. ale co to znaczy, ze 
# POPRAWNIE otrzymamy taka p-wartosc? to znaczy, ze faktycznie
# hipoteza zerowa jest falszywa. dlaczego? pomyslmy, co sie dzieje, 
# gdy dostajemy p-wartosc mniejsza od \alpha -- odrzucamy wtedy hipoteze
# zerowa! czyli -- jeszcze raz -- POPAWNA p-wartosc < \alpha to taka,
# ze gdy odrzucamy H0 (bo p < \alpha) to rzeczywiscie jest tak,
# ze H0 jest falszywa. a tak wlasnie jest u nas!
# stad tez jest pomysl na symulacje. skoro mamy hipoteze alternatywna 
# prawdziwa, to kazda wartosc p < \alpha bedzie poprawna. z tego
# tez powodu zliczamy, ile ich (p-wartosci < \alpha) jest 
# (dla kazdej zmiennej osobno!) i dzielimy przez liczbe symulacji.
# pozwala nam to wyestymowac moc.