### Zadanie 1 ###
air = read.table("airpollution.txt", header = T)
air

### a)
m1 <- lm(Mortality ~ NOx, data = air)
summary(m1)$coef

plot(air$Mortality ~ air$NOx)
abline(m1)

summary(m1); summary(m1)$r.squared # R62 = 0.006
# model nie opisuje dobrze danych

### b)
m2 <- lm(Mortality ~ log(NOx), data = air)
plot(air$Mortality ~ log(air$NOx))
abline(m2)

summary(m2)$coef
summary(m2)
# R2 = 0.09 -- bardzo male, model kiepsko opisuje dane

### c)
rstandard(m2) # studentyzowane
rstudent(m2) # studentyzowane modyfikowane
rstandard(m2)[abs(rstandard(m2)) > 2] # indeksy obserwacji
rstudent(m2)[abs(rstudent(m2)) > 2] # indeksy obserwacji
# otrzymujemy, ze obserwacje 29,37,47,49 sa potencjalnie odstajace

m3 <- lm(Mortality ~ log(NOx), data = air, subset = -c(29,37,47,49))
# to jest model bez tych obserwacji
summary(m3) ## R2 = 0.33, jest troszke lepiej
summary(m2)

plot(air$Mortality ~ log(air$NOx))

# funkcja text pozwala nam namiesc indeksy obserwacji na wykres
# pos = 1 -- etykieta pod punktem
text(log(air$NOx), air$Mortality, labels = 1:length(air$NOx), pos = 1,
     cex = 1)
abline(m3)
abline(m2)

# wykres bez obs. potencjalnie odstajacych
plot(air$Mortality[-c(29,37,47,49)] ~ log(air$NOx)[-c(29,37,47,49)])

### Zadanie 2

### a)
p <- read.table("phila.txt", header = T)
wm <- which((is.na(p$CrimeRate))) # indeksy brakow danych w zmiennej CrimeRate
p <- p[-wm,]
row.names(p) <- 1:nrow(p) ## resetowanie indeksu!

### b)
mod <- lm(HousePrice ~ CrimeRate, data = p)
plot(p$HousePrice ~ p$CrimeRate, pch = 16) # pch = 16 to punkt zamalowany
abline(mod) 
summary(mod)
# kiepskie dopasowanie modelu mimo istotnosci zmiennej CrimeRate

# jak znalezc obserwacje "maksymalna"?
text(p$CrimeRate, p$HousePrice, labels = 1:length(p$CrimeRate), pos = 1,
     cex = 1) 
which(p$CrimeRate > 350) # indeks obserwacji wiekszej niz 350
which(p$CrimeRate == max(p$CrimeRate)) # indeks obserwacji maksymalnej

# czy na pewno jest to obserwacja wplywowa lub odstajaca?
# wplywowosc:
# heurystyka 1: h > 2(p/n)
# p = 2 bo intercept i x
h <- hatvalues(mod)
as.numeric(which(h > 2*2/nrow(p))) ## obserwacja 63

# heurystyka 2: Cook
Cook = cooks.distance(mod)
Cook[Cook == max(Cook)] # 28.81
as.numeric(which(Cook == max(Cook))) # obserwacja 63, bez "etykiety"

# odstawanie:
rstandard(mod) ### rezydua studentyzowane
rstudent(mod) ### rezydua studentyzowane modyfikowane

as.numeric(which(abs(rstandard(mod)) > 2)) ## m.in obs 63
as.numeric(which(abs(rstudent(mod)) > 2)) ## m.in obs 63

# zatem zarowno wplywowa, jak i odstajaca, jest obs. 63

plot(p$CrimeRate[-63], p$HousePrice[-63])

mod2 <- lm(HousePrice ~ CrimeRate, data = p, subset = -63)

abline(mod2)

# czy wspolczynniki sie zmienily?
summary(mod2)$coef; summary(mod)$coef

# R2 = 0.18, model niezbyt dobrze opisuje dane 

