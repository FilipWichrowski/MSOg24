### Zad 1 ###

SA <- read.table("SAheart.data", sep = ",", h = T)

SA <- SA[,-1]
# pierwsza kolumna to indeksy obserwacji

### a)

SA.logit <- glm(chd ~ ., data = SA, family = "binomial")
# family = "binpmial" dla regresji logistycznej

### b)
summary(SA.logit)
# istotne zmienne: tobacco, ldl, famhistPresent, typea, age.
# czy widzimy cos tutaj zaskakujacego? wspolczynnik dla zmiennej
# obesity ma wartosc ujemna, czyli znaczyloby to, ze wzrostem wagi 
# zwiazana jest mniejsza szansa na zawal. musimy pamietac (por. pdf 
# o wspolliniowosci zmiennych i metodach regularyzacji ktory wstawilem), 
# ze jednym z symptomow wspolliniowosci jest wlasnie przeciwny znak wspolczynnika
# niz spodziewany (np. z teorii) -- mamy skorelowane zmienne ze zmienna obesity,
# ktore przejmuja na siebie ciezar wyjasnienia zmiennej odpowiedzi, a to, co 
# "zostaje" dla obesity, moze wlasnie byc niespodziewane.

### c)
exp(as.numeric(coef(SA.logit)["age"]))
# szansa jest 1.05 raza wieksza

### d)
SA.logit.aic <- step(SA.logit, direction = "backward", k = 2) # selekcja z uzyciem kryterium AIC
summary(SA.logit.aic)
SA.logit.bic <- step(SA.logit, direction = "backward", k = log(nrow(SA))) # selekcja z uzyciem kryterium BIC
summary(SA.logit.bic)

### e)
SA.logit_null <- glm(chd ~ 1, data = SA, family = "binomial")
# model pusty

anova(SA.logit_null, SA.logit)
anova(SA.logit_null, SA.logit, test = "Chisq") # tu od razu p-wartosc
pv = 1 - pchisq(123.97, df = 9)
pv
# p-wartosc jest ponizej poziomu istotnosci 0.05, w zwiazku z czym odrzucamy 
# H0 o tym, ze mniejszy model jest adekwatny, czyli w naszym modelu sa istotne zmienne

### f)
SA2 <- SA
SA2$age2 <- (SA$age)^2
SA.logit2 <- glm(chd ~ ., data = SA2, family = "binomial")

anova(SA.logit, SA.logit2, test = "Chisq")
pv = 1 - pchisq(0.88, df = 1)
# p-wartosc jest wieksza, niz poziom istotnosci 0.05, w zwiazku z tym nie odrzucamy
# H0, ze \beta_{age} = 0; zatem zmienna nie jest istotna.


