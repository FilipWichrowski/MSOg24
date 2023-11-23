############### LISTA 5 ############### 

s = read.table("savings.txt", header = T)
mod <- lm(Savings ~ . -Country, data = s, subset = -49)

### podpunkt b)
summary(mod) # zmienna dpi nie jest istotna, bo p > 0.05

# wykres czesciowej regresji dla zmiennej dpi
eydpi <- lm(Savings ~ .-Country-dpi, data = s, subset = -49)$res
exdpi <- lm(dpi ~ .-Country-Savings, data = s, subset = -49)$res
plot(eydpi ~ exdpi)
abline(lm(eydpi ~ exdpi)) 
# zmienna dpi raczej nie wnosi nic nowego przy obecnosci innych zmiennych w modelu

# wykres czesciowej regresji dla zmiennej ddpi
eyddpi <- lm(Savings ~ .-Country-ddpi, data = s, subset = -49)$res
exddpi <- lm(ddpi ~ .-Country-Savings, data = s, subset = -49)$res
plot(eyddpi ~ exddpi)
text(exddpi, eyddpi)
abline(lm(eyddpi ~ exddpi)) 
# wydaje sie, ze zmienna ddpi wnosi dodatkowa informacje, zatem gdyby byla
# np. zmienna kandydujaca, to prawdopodobnie warto byloby ja wlaczyc do analizy

### podpunkt c)
cor(s$Pop15, s$Pop75) # ujemnie skorelowane zmienne (-0.91)
summary(mod, cor = T) # dodatnio skorelowane wspolczynniki (0.79)
# warto poczytac: confounding variable, paradoks Simpsona

### podpunkt d)
# wykres czesciowych rezyduow
library(faraway)
prplot(mod, 1) # widzimy dwie grupy: jedna dla Pop15 < 35, druga dla Pop15 > 35

# z definicji: e + x_i * beta_i vs x_i
plot(mod$res + s$Pop15[-49]*mod$coef[2] ~ s$Pop15[-49]) # bez obs. 49

# zobaczymy, co sie dzieje w tych podgrupach (wlacznie z obs. 49):
lm(Savings ~ .-Country, data = s, subset = Pop15 < 35)$coef["Pop15"]
lm(Savings ~ .-Country, data = s, subset = Pop15 > 35)$coef["Pop15"]


############### LISTA 6 ############### 

usc = read.table("uscrime.txt", header = T)

### Zadanie 1 ### 

### podpunkt a)
mod_usc <- lm(R ~ ., data = usc)
summary(mod_usc)

pairs(usc) # wykresy rozproszenia 
cor(usc) # wspolczynniki korelacji

# jak znalezc zmienne najbardziej skorelowane? skorzystamy z funkcji
# melt() z biblioteki reshape2
library(reshape2)
cors <- subset(melt(cor(usc)), value < 1 & value > 0.8) # odciecie wg uznania
cors[order(cors$value, decreasing = T), ] # najabrdziej skorelowane Ex0 i Ex1
# usunmy zatem np. zmienna Ex0

mod_usc2 <- lm(R ~ . -Ex0, data = usc)

### podpunkt b)
n = nrow(usc)
null_mod <- lm(R ~ 1, data = usc) # model tylko z interceptem

### AIC
backward_aic <- step(mod_usc2, direction = "backward", k = 2) # startujemy z pelnego modelu
both_aic <- step(mod_usc2, direction = "both", k = 2)
forward_aic <- step(null_mod, direction = "forward", k = 2,
                    scope = list(lower = null_mod, upper = mod_usc2)) 
# uwaga do forward: zaczynamy z pustego modelu, ale musimy podac model "upper", 
# poniewaz inaczej funkcja nie step() nie bedzie wiedziala, jakie zmienne jeszcze sa

### BIC
backward_bic <- step(mod_usc2, direction = "backward", k = log(n))
both_bic <- step(mod_usc2, direction = "both", k = log(n))
forward_bic <- step(null_mod, direction = "forward", k = log(n),
                    scope = list(lower = null_mod, upper = mod_usc2))

# Pomocna uwaga: zalozmy, ze z jakiegos powodu chcemy NA PEWNO miec w modelu
# zmienna np. NW. wtedy mozemy przekazac nastepujacy model:
lower_model <- lm(R ~ NW, data = usc)
# do argumentu scope:
step(mod_usc2, direction = "both", k = log(n), scope = list(lower = lower_model))
# R ~ Age + Ed + Ex1 + NW + U2 + X
# widzimy, ze mamy zmienna NW zawarta w naszym finalnym modelu
step(mod_usc2, direction = "both", k = log(n), scope = list(lower = null_mod))
# R ~ Age + Ed + Ex1 + U2 + W + X

### podpunkt c)

tvals <- summary(mod_usc2)$coef[,3][-1] # bierzemy t-wartosci poza interceptem 
tvals_ord <- sort(abs(tvals), decreasing = T) # sortujemy malejaco po modulach t
features <- names(tvals_ord) # nazwy zmiennych
p <- length(features) # liczba wszystkich predyktorow

mods <- list()
aic <- numeric(p)
bic <- numeric(p)
sse <- numeric(p)
n <- nrow(usc)

for(i in 1:p)
{
  m <- as.formula(paste("R ~ ", paste(features[1:i], collapse = "+"))) 
  mods[[i]] <- lm(m, data = usc)
  sse[i] <- sum(mods[[i]]$res^2)
  aic[i] <- n*log(sse[i]/n) + 2*i
  bic[i] <- n*log(sse[i]/n) + i*log(n)
}
plot(1:p, aic, col = "red", type = "b")
lines(1:p, bic, col = "blue", type = "b")
legend("bottomleft", c("AIC","BIC"), col = c("red", "blue"), lty = c(1, 1))


### Zadanie 2 ### 

library(MASS) # dla mvrnorm
N <- c(25, 50, 75, 100, 125, 150, 175, 200)
PppwAIC <- numeric() # prawdopodobienstwo poprawnie wybranego modelu, AIC
PppwBIC <- numeric() # prawdopodobienstwo poprawnie wybranego modelu, BIC


for(ni in N)
{
  ppwAIC <- numeric() # czy poprawnie wybrany model, AIC
  ppwBIC <- numeric() # czy poprawnie wybrany model, BIC
  for(powt in 1:100)
  {
    # x = mvrnorm(n = ni, rep(0, 9), diag(1, 9, 9), empirical = T) albo:
    x = matrix(0, nrow = ni, ncol = 9)
    for(j in 1:9) x[,j] = rnorm(ni)
    
    eps = rnorm(ni)
    b = c(rep(1,3), rep(0, 6))
    y = x %*% b + eps 
    d = data.frame(x, y)
    mod = lm(y ~ ., data = d)
    modBIC = step(mod, direction = "backward", k = log(ni))
    modAIC = step(mod, direction = "backward", k = 2)
    czyokBIC = setequal(names(coef(modBIC))[-1], c("X1","X2","X3"))
    czyokAIC = setequal(names(coef(modAIC))[-1], c("X1","X2","X3"))
    # sprawdzamy, czy model z funkcji step() "pokrywa" sie z prawdziwym modelem
    ppwBIC[powt] = ifelse(czyokBIC, 1, 0) 
    ppwAIC[powt] = ifelse(czyokAIC, 1, 0)
    # jesli model z funkcji step() sklada sie z X1,X2,X3 -- tak, jak 
    # prawdziwy model, to wtedy przypisujemy jedynke
  }
  ind = which(ni == N)
  PppwAIC[ind] <- sum(ppwAIC)/length(ppwAIC)
  PppwBIC[ind] <- sum(ppwBIC)/length(ppwBIC)
  # powyzej estymujemy prawdopodobienstwo wyboru poprawnego modelu
}

plot(N, PppwAIC, col = "red", type = "b", ylim = c(0, 1))
lines(N, PppwBIC, col = "blue", type = "b")
legend("bottomleft", c("AIC","BIC"), col = c("red", "blue"), lty = c(1, 1))

### przydatna funkcja do generowania np. niezaleznych wektorow z N(0,1):

Xemp = mvrnorm(n = ni, mu = rep(0, 9), Sigma = diag(1, 9, 9), empirical = T)
Xnotemp = mvrnorm(n = ni, mu = rep(0, 9), Sigma = diag(1, 9, 9), empirical = F)

apply(Xemp, 2, mean)
apply(Xnotemp, 2, mean)

apply(Xemp, 2, sd)
apply(Xnotemp, 2, sd)

# trzeba uwazac na argument "empirical"
