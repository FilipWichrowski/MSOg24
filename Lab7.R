##### Zadanie 1 ##### 
### a)
data(trees)
mod1 <- lm(Volume ~ Girth, data = trees)
mod2 <- lm(Volume ~ Girth + Height, data = trees)
mod3 <- lm(Volume ~ Girth + Height + I(Height*Height), data = trees)
# pewne klasy przeksztalcen predyktorow (np. potegowe) musza byc wykonane
# z uzyciem funkcji I() (por. ?I), czyli "as.is"
# np. lm(Volume ~ Girth + Height + I(Height*Height), data = trees)
# np. lm(Volume ~ Girth + Height + I(Height^0.3), data = trees)
# np. lm(Volume ~ Girth + Height + I(2*Height), data = trees)

anova(mod2, mod1, test = "F") 
# p-wartosc = 0.015 < 0.05: odrzucamy H0, czyli model mniejszy nieadekwatny
anova(mod3, mod2, test = "F") 
# p-wartosc = 0.075 > 0.05: nie odrzucamy H0, czyli model mniejszy adekwatny

### b)
sse1 <- sum(mod1$residuals^2)
sse3 <- sum(mod3$residuals^2)
df1 <- mod1$df.residual
df3 <- mod3$df.residual
anova(mod1, mod3, test = "F")
Fstat = (sse1 - sse3)/(df1 - df3)*df3/sse3
1 - pf(Fstat, df1 - df3, df3) 
# p-wartosc 0.011 < 0.05: model mniejszy nie jest adekwatny

# stopnie swobody:
df1 == nrow(trees) - length(coef(mod1))
df3 == nrow(trees) - length(coef(mod3))

##### Zadanie 2 #####

library(car)
?linearHypothesis

### a)
data(Davis)

mod.davis <- lm(weight ~ repwt, data = Davis)
summary(mod.davis)

# chcemy testowac: intercept = 0, repwt = 1
# wszystkie 3 ponizsze hipotezy sa rownowazne
linearHypothesis(mod.davis, hypothesis.matrix = diag(2), rhs = c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
# p = 0.1793 > 0.05, zatem nie odrzucamy H0; nie jestesmy w stanie stwierdzic,
# z intercept != 0 i reptw != 1

# mozemy tez testowac np. rownosc wspolczynnikow
linearHypothesis(mod.davis, "(Intercept) = repwt ")

### b)
c = read.table("ExerciseCholesterol.txt")

# one-hot encoding grupy:
i1 = ifelse(c$Group == 1, 1, 0)
i2 = ifelse(c$Group == 2, 1, 0)
i3 = ifelse(c$Group == 3, 1, 0)
s1 = numeric(nrow(c)); s2 = numeric(nrow(c)); s3 = numeric(nrow(c))
# transformacja zmiennej Weight do podgrup
s1[c$Group == 1] = c$Weight[c$Group == 1]
s2[c$Group == 2] = c$Weight[c$Group == 2]
s3[c$Group == 3] = c$Weight[c$Group == 3]

d = data.frame(i1, i2, i3, s1, s2, s3, c$HDL)
names(d)[7] <- "HDL"

# szybszy sposob (inspirowany pomyslem Jakuba Grzywaczewskiego) na uzyskanie takiej ramki:
i1 = ifelse(c$Group == 1, 1, 0)
i2 = ifelse(c$Group == 2, 1, 0)
i3 = ifelse(c$Group == 3, 1, 0)
i <- cbind(i1,i2,i3)
d <- data.frame(i1, i2, i3, i*c$Weight, c$HDL)
names(d)[4:7] <- c("s1", "s2", "s3", "HDL")

# wracamy do zadania
mod <- lm(HDL ~ .-1, data = d)
# nasza zmienna "Group" ma trzy poziomy. do zakodowania one-hot zmiennej o "k"
# poziomach, potrzebujemy tylko "k-1" zmiennych binarnych. dlaczego? 
# my zakodowalismy tak
# 1      1 0 0
# 1      1 0 0
# 2   =  0 1 0
# 2      0 1 0
# 2      0 1 0
# 3      0 0 1
# ALE ta trzecia kolumna jest zbedna, poniewaz jej wartosci sa zdeterminowane
# przez dwie pierwsze kolumny (DUMMY VARIABLE TRAP): w ostatnim wierszu mamy
# dwa zera, wiec wiemy, ze wtedy w trzeciej kolumnie bedzie jedynka. nie ma zatem
# koniecznosci dodawania tej trzeciej kolumny. jesli jednak ja dodamy, to musimy
# wyrzucic z modelu intercept, poniewaz intercept jest liniowo zalezny od tych 
# trzech kolumn, ktore mamy powyzej.


coef(mod) # niech te wspolczynniki nazywaja sie b0, g0, d0, b1, g1, d1
# testujemy hipoteze b1 = g1 i b1 = d1, czyli 
# b1 - g1 = 0
# b1 - d1 = 0
A = matrix(0, ncol = 6, nrow = 2)
A[1,] = c(0, 0, 0, 1, -1, 0)
A[2,] = c(0, 0, 0, 1, 0, -1)
linearHypothesis(mod, A, c(0, 0))
# p = 0.075, zatem nie mozemy odrzucic hipotezy, ze wszystkie nachylenia 
# sa takie same

# WAZNE: po wyrzuceniu interceptu mamy tak zwane kodowanie LMC, czyli kodowanie 
# srednimi. oznacza to, ze wspolczynniki i1, i2, i3 to ABSOLUTNE wartosci naszych 
# interceptow. spojrzmy teraz na model, gdzie zmienna Group jest kodowana "poprawnie",
# czyli bez np. pierwszej kolumny i1. nie musimy wyrzucac interceptu, poniewaz
# nie ma problemu z liniowa zaleznoscia

d2 <- data.frame(i2, i3, i*c$Weight, c$HDL)
names(d2)[3:6] <- c("s1", "s2", "s3", "HDL")

mod2 <- lm(HDL ~ ., data = d2)
coef(mod2)

# tutaj mamy kodowanie RLC, ktore swoja droga jest domyslne w R dla zmiennych 
# kategorycznych. co ono oznacza? otoz pierwszy intercept, tak jak w poprzednim 
# modelu "mod", jest wartoscia srednia dla pierwszego poziomu odniesienia, czyli int1.
# ta wartosc jest teraz naszym POZIOMEM REFERENCYJNYM (RLC - Reference Level Coding)
coef(mod)[1]; coef(mod2)[1]
# dwa kolejne intercepty roznia sie:
coef(mod)[2]; coef(mod2)[2]
coef(mod)[3]; coef(mod2)[3]
# z czego to wynika? otoz w kodowaniu RLC kodowana jest ROZNICA miedzy danym poziomem
# a poziomem referencyjnym, czyli
coef(mod2)[2]
# jest roznica miedzy pierwszym (referencyjnym) a drugim interceptem, zatem srednia 
# wartosc interceptu int2 jest rowna
coef(mod2)[1] + coef(mod2)[2]; coef(mod)[2]
# podobnie, dla trzeciego interceptu, kodowana jest roznica miedzy tym poziomem 
# a poziomem referencyjnym, zatem
coef(mod2)[1] + coef(mod2)[3]; coef(mod)[3]
# wszystko sie zgadza :))

# tutaj wizualizacja wspolczynnikow nachylenia w zaleznosci od grupy:
library(ggplot2)
c$Group <- as.factor(c$Group)
ggplot(data = c, aes(x = Weight, y = HDL, color = Group)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)


### DLA CHETNYCH (warto):
# wiemy, ze ogolna hipoteza liniowa testowana jest za pomoca testu F. jak policzyc
# recznie wartosc takiej statystyki? ponownie przydatna jest funkcja I()
mod3 <- lm(HDL ~ i1 + i2 + I(s1 + s2 + s3), data = d)
# napisanie I(s1 + s2 + s3) oznacza, ze mamy warunek, ze nachylenia dla s1, s2, s3 
# maja byc identyczne. wtedy
anova(mod3, mod) 
# dostajemy taka sama p-wartosc (0.07542) jak z funkcji linearHypothesis. 
# mozna tez recznie:

sseA <- sum(mod3$residuals^2)
sseB <- sum(mod$residuals^2)
dfA <- mod3$df.residual
dfB <- mod$df.residual

FstatAB <- (sseA - sseB)/(dfA - dfB) * dfB/sseB
1 - pf(FstatAB, dfA - dfB, dfB) # zgadza sie z anova i linearHypothesis()

# jesli chcielibysmy na przyklad testowac, ze b1 = -d1 (zdefiniowane tak, jak
# w linijce 96.)
mod4 <- lm(HDL ~ i1 + i2 + I(s1 - s3) + s2, data = d)
coef(mod4)
anova(mod4, mod) # wyszloby, ze nie mozemy odrzucic hipotezy, ze b1 = -d1

### DLA CHETNYCH 2 (moze pogadamy kiedys o tym):
# cala analize da sie przeprowadzic znacznie szybciej, wprowadzajac do modelu
# interakcje poprzez operator *
# krotko czym sa interakcje (pojawia sie na pewno przy okazji ANOVY):
# najlatwiej jest myslec o interakcjach w kategoriach zmiennych dyskretnych; 
# zalozmy, ze mamy dwie zmienne kategoryczne (np. plec (x1) i wiek (x2), z np. trzema poziomami dla wieku) 
# i sprawdzamy, w jaki sposob odnosza sie one do pewnej zmiennej zaleznej, np. zarobkow. 
# wyraz x1 * x2, jesli jest znaczacy, mozna tutaj rozumiec jako zarobki zachowujace sie 
# inaczej na roznych poziomach wieku dla roznych plci. na przyklad: byc moze zarobki sa 
# stabilne dla mezczyzn w trzech grupach wiekowych, ale mlode kobiety zaczynaja ponizej mlodych 
# mezczyzn i maja tendencje wzrostowa (przy czym starsza grupa wiekowa dla kobiet ma 
# wyzsze srednie zarobki, niz starsza grupa wiekowa dla mezczyzn). 
# na wykresie srednich oznaczaloby to pozioma linie dla mezczyzn w srodku wykresu 
# i byc moze linie 45 stopni dla kobiet, ktora zaczyna sie ponizej mezczyzn, ale konczy powyzej 
# mezczyzn. istota jest to, ze w miare przesuwania sie wzdluz poziomow jednej zmiennej 
# (utrzymywania np. x1, czyli plci, na stalym poziomie), zmienia sie to, co dzieje sie w drugiej zmiennej. 
# zwykly model liniowy bez interakcji nie pokaze nam tego zachowania: interpretacja modelu:
# zarobki = b0 + b1*plec + b2*wiek
# jest przeciez taka, ze b1 mowi nam o sredniej zmianie zarobkow gdy zwiekszymy plec
# o 1 jednostke, a wiek bedzie na stalym poziomie. tak samo b2 powie nam o sredniej zmianie
# zarobkow, gdy zwiekszymy wiek o 1 jednostke, a plec bedzie na stalym poziomie. nie dowiemy
# sie wiec jak marginalny wplyw wieku na zarobki rozni sie miedzy kobietami i mezczyznami: do tego
# potrzebujemy modelu z interakcjami.


modINT <- lm(HDL ~ as.factor(Group)*Weight, data = c)
# musimy podac grupe jako zmienna kategoryczna -- inaczej bedzie traktowana jako
# zmienna ciagla, co nie ma sensu (dlaczego?). uwaga: dla zmiennych binarnych 
# nie trzeba podawac jako as.factor() -- przy dwoch wartosciach nie ma to znaczenia.
# model powyzej to tak naprawde model HDL ~ G + W + G*W, gdzie G*W oznacza po prostu
# mnozenie dwoch zmiennych. zobaczmy na wspolczynniki
coef(modINT)
# tak, jak napisalem wczesniej, domsylnym kodowaniem w R dla zmiennych kategorycznych
# jest kodowanie RLC. co to znaczy? wartoscia REFERENCYJNA dla grupy jest 
# wartosc 23.054. pozostale wartosci sa wyrazone jako roznica, czyli
# dla grupy 2 wartosc srednia to 23.054 - 8.799 = 14.255 (jak wczesniej)
# dla grupy 3 wartosc srednia to 23.054 + 53.825 = 76.879  (jak wczesniej)
# podobnie jest z nachyleniami: 
# b1 = 0.2496 (poziom REFERENCYJNY)
# g1 = 0.2495 + 0.0014 = 0.2509 (jak wczesniej)
# d1 = 0.2495 - 0.3317 = -0.0821 (jak wczesniej)
# trzeba o tym pamietac! jeszcze raz: np. as.factor(Group)2:Weight
# oznacza ROZNICE miedzy poziomem REFERENCYJNYM a rozpatrywanym poziomem,
# czyli jesli chcielismy sprawdzic, czy trzy nachylenia sa takie same, to tutaj
# mozemy testowac czy ROZNICE NACHYLEN sa rowne 0, czyli

A = matrix(0, ncol = 6, nrow = 2)
A[1,] = c(0, 0, 0, 0, 1, 0)
A[2,] = c(0, 0, 0, 0, 0, 1)
linearHypothesis(modINT, A, c(0, 0)) # p-wartosc wychodzi dokladnie taka sama

