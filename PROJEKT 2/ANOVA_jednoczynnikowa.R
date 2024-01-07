## Rozpatrujemy nastêpuj¹cy zbiór danych
##  Grupa1 | 2.8, 3.6, 3.4, 2.3
##  ------------------------
##  Grupa2 | 5.5, 6.3, 6.1, 5.7
##  ------------------------
##  Grupa3 | 5.8, 8.3, 6.9, 6.1

y<- c(2.8, 3.6, 3.4, 2.3,
	5.5, 6.3, 6.1, 5.7,
	5.8, 8.3, 6.9, 6.1) 

# Pytanie: czy istnieje istotna ró¿nica pomiêdzy œrednimi w grupach? 

grupy <- factor(c(1,1,1,1,
			2,2,2,2,
			3,3,3,3))
#factor sprawia, ¿e grupy nie s¹ postrzegane jako wartoœci liczbowe
#Domyœlnym wykresem jest wówczas wykres pude³kowy (box plot)
#Wyci¹gamy wnioski o "umiejscowieniu" danych i jednorodnoœci wariancji
plot(grupy,y)

##Przyk³ad 1.
 
#œrednia ogólna \hat{\mu}
mu<-mean(y)
mu
#œrednie w grupach 
mui<-tapply(y,grupy,mean)
mui
#estymator \hat{\alpha}_i
alpha<-mui - mu
alpha

##Przyk³ad 2. Dekompozycja zmiennoœci ca³kowitej.
SST <- sum((y - mu)^2)
SSW <- sum((y[grupy==1] - mui[1])^2)+sum((y[grupy==2] - mui[2])^2)+sum((y[grupy==3] - mui[3])^2)
SSB <- 4 * sum(alpha^2)
c(SST, SSW, SSB)
#Mo¿na równie¿ wyznaczyæ SSW ze œrednich wariancji w grupch
vars <- tapply(y, grupy, var)
(12-3)*mean(vars)

##Przyk³ad 3. 
F <- (SSB/(3 - 1)/(SSW/(12 - 3)))
pval <- 1 - pf(F, df1 = 3 - 1, df2 = 12 - 3)
c(F , pval)

#F=26.7, p_val=0.000165, odrzucamy H_0 na poziomie istotnoœci np. 0.05. 

##Analizê mo¿na przeprowadziæ bezpoœrednio z R
anova(lm(y ~ grupy))
model<-aov(y ~grupy)
summary(model)

##Przyk³ad 4. 
#Porównamy œrednie w Grupie1 i w Grupie2.
#Wyznaczamy 95% przedzia³ ufnoœci (poziom ufnoœci 0.05). 
mui[1] - mui[2] + c(-1, 1)*qt(0.975,df=12-3)*sqrt(SSW/(12-3)*(1/4+1/4))
#Wyznaczamy p-wartoœæ
tobl <-(mui[1] - mui[2])/sqrt(SSW/(12 - 3) * (1/4 + 1/4))
2*(1 - pt(abs(tobl), 9))
#A wiêc grupa 1 ró¿ni siê istotnie od grupy 2.

#Trzeba wykonaæ takich porównañ 3*2/2 = 3.

#stosujemy test post hoc 
TukeyHSD(model)
#graficzna prezentacja wyników testu Tukey'a
plot(TukeyHSD(model))


## Analiza za³o¿eñ
#wykres kwantyl-kwantyl
reszty <- lm(y ~ grupy)$residuals
qqnorm(reszty)
qqline(reszty)