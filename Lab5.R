####################### Lista 4 ####################### 
### Zadanie 3)
### a)
n <- 30
x1 <- runif(n, 0, 10)
x2 <- runif(n, 10, 20)
x3 <- runif(n, 20, 30)
x <- c(x1, x2, x3)

eps1 <- rnorm(n, 0, 1)
eps2 <- rnorm(n, 0, 3)
eps3 <- rnorm(n, 0, 5)
eps <- c(eps1, eps2, eps3)
y <- x + eps

plot(x, y) # lub plot(y ~ x)
mod <- lm(y ~ x)
abline(mod)

### b)

plot(1:(3*n), rstudent(mod))
# widzimy, ze rozrzut rezyduow zzmienia sie wraz z indeksem obserwacji
# zatem mozliwa heteroskedastycznosc

### c)
rez <- residuals(mod)
w <- 1 / lm(abs(rez) ~ mod$fitted.values)$fitted.values^2
mod1 <- lm(y ~ x, weights = w)
plot(1:(3*n), rstudent(mod1))
# pozbylismy sie heteroskedastycznosci

### d)
x <- c(x, 5)
y <- c(y, 10)

####################### Lista 5 ####################### 
### Zadanie 1)

a = read.table("cellular.txt", header = T)

m1 = lm(Subscribers ~ Period, data = a)

plot(a$Subscribers ~ a$Period) # ewidentnie zaleznosc nie jest liniowa
abline(m1)
summary(m1)

# diagnostyka 

plot(m1, 1) # taki wykres rezyduow sugeruje potencjalna koniecznosc przetransformowania
            # zmiennej odpowiedzi (np. y^0.25, y^0.5) lub predyktora (np. x^2)
plot(m1, 2) # obserwacja 23 wydaje sie delikatnie odstawac, dodatkowo qq plot 
            # wskazuje na prawostronna skosnosc rozkladu rezyduow (a wiec odstepstwo 
            # od normalnosci)
plot(m1, 3) # obs 23 jest potencjalnie odstajaca, ponownie koniecznosc transformacji
            # zmiennej/zmiennych
plot(m1, 4) # obs 23 jest potencjalnie wplywowa
plot(m1, 5) # obs 23 jest potencjalnie wplywowa
plot(m1, 6) # obs 23 jest potencjalnie wplywowa


### obserwacje wplywowe:
X = model.matrix(m1)
n = nrow(X)
p = ncol(X)
c = cooks.distance(m1)
# przykladowa heurystyka
as.numeric(which(c > 4/(n-p)))

# duza dzwignia:
h = hatvalues(m1)
as.numeric(which(h > 2*p/n))

# obserwacje odstajace:
as.numeric(which(abs(rstandard(m1)) > 2))
as.numeric(which(abs(rstudent(m1)) > 2))


### b)
l2 <- lm(log(Subscribers) ~ Period, data = a)
l3 <- lm((Subscribers)^0.5 ~ Period, data = a)
l4 <- lm((Subscribers)^0.25 ~ Period, data = a)

summary(l2)
summary(l3)
summary(l4)

plot(log(a$Subscribers) ~ a$Period)
abline(l2)
plot((a$Subscribers)^0.5 ~ a$Period)
abline(l3)
plot((a$Subscribers)^0.25 ~ a$Period)
abline(l4)
# WIZUALNIE najlepiej dopasowany wydaje sie byc model l4 z transformacja (a$Subscribers)^0.25,
# jednak widoczne sa oscylacje (systematyczne odchylki) wartosci dookola prostej mnk, 
# co wskazuje na fakt, ze prawdziwa zaleznosc moze nie byc liniowa
