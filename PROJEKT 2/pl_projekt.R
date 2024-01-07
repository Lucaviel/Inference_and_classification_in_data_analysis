#losowanie 15 wartosci o rozkladzie normalnym
set.seed(15052022)
round(rnorm(15,10.2, 3.8),2)

A <- read.csv('dane13.csv', header = TRUE, row.names = NULL, sep = ';')

#rozdzielanie danych na 4 kategorie
y <- as.numeric(gsub(",", ".", A[,1]))
grupy <- factor(as.numeric(A[,2]))

#Dla poszczególnych 1-4 grup:
#wartosc minimalna, pierwszy kwartyl, mediana, srednia, trzeci kwartyl, wartosc maksymalna
tapply(y, grupy, summary)

#wariancja dla kazdej grupy
vars <- tapply(y, grupy, var)

#wykres pudelkowy dla kazdej z grup
plot(grupy, y)

#tabela jednoczynnikowej analizy wariancji
anova(lm(y ~ grupy))
model<-aov(y ~grupy)
summary(model)

#test poc hoc
TukeyHSD(model)
plot(TukeyHSD(model))

#analiza zalozen ANOVA wykorzystamy wykres kwantyl-kwantyl
reszty <- lm(y ~ grupy)$residuals
qqnorm(reszty)
qqline(reszty)

bartlett.test(y,grupy)
