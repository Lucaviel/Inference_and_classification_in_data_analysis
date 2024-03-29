## Rozpatrujemy nast�puj�cy zbi�r danych
##  Grupa1 | 2.8, 3.6, 3.4, 2.3
##  ------------------------
##  Grupa2 | 5.5, 6.3, 6.1, 5.7
##  ------------------------
##  Grupa3 | 5.8, 8.3, 6.9, 6.1

y<- c(2.8, 3.6, 3.4, 2.3,
	5.5, 6.3, 6.1, 5.7,
	5.8, 8.3, 6.9, 6.1) 

# Pytanie: czy istnieje istotna r�nica pomi�dzy �rednimi w grupach? 

grupy <- factor(c(1,1,1,1,
			2,2,2,2,
			3,3,3,3))
#factor sprawia, �e grupy nie s� postrzegane jako warto�ci liczbowe
#Domy�lnym wykresem jest w�wczas wykres pude�kowy (box plot)
#Wyci�gamy wnioski o "umiejscowieniu" danych i jednorodno�ci wariancji
plot(grupy,y)

##Przyk�ad 1.
 
#�rednia og�lna \hat{\mu}
mu<-mean(y)
mu
#�rednie w grupach 
mui<-tapply(y,grupy,mean)
mui
#estymator \hat{\alpha}_i
alpha<-mui - mu
alpha

##Przyk�ad 2. Dekompozycja zmienno�ci ca�kowitej.
SST <- sum((y - mu)^2)
SSW <- sum((y[grupy==1] - mui[1])^2)+sum((y[grupy==2] - mui[2])^2)+sum((y[grupy==3] - mui[3])^2)
SSB <- 4 * sum(alpha^2)
c(SST, SSW, SSB)
#Mo�na r�wnie� wyznaczy� SSW ze �rednich wariancji w grupch
vars <- tapply(y, grupy, var)
(12-3)*mean(vars)

##Przyk�ad 3. 
F <- (SSB/(3 - 1)/(SSW/(12 - 3)))
pval <- 1 - pf(F, df1 = 3 - 1, df2 = 12 - 3)
c(F , pval)

#F=26.7, p_val=0.000165, odrzucamy H_0 na poziomie istotno�ci np. 0.05. 

##Analiz� mo�na przeprowadzi� bezpo�rednio z R
anova(lm(y ~ grupy))
model<-aov(y ~grupy)
summary(model)

##Przyk�ad 4. 
#Por�wnamy �rednie w Grupie1 i w Grupie2.
#Wyznaczamy 95% przedzia� ufno�ci (poziom ufno�ci 0.05). 
mui[1] - mui[2] + c(-1, 1)*qt(0.975,df=12-3)*sqrt(SSW/(12-3)*(1/4+1/4))
#Wyznaczamy p-warto��
tobl <-(mui[1] - mui[2])/sqrt(SSW/(12 - 3) * (1/4 + 1/4))
2*(1 - pt(abs(tobl), 9))
#A wi�c grupa 1 r�ni si� istotnie od grupy 2.

#Trzeba wykona� takich por�wna� 3*2/2 = 3.

#stosujemy test post hoc 
TukeyHSD(model)
#graficzna prezentacja wynik�w testu Tukey'a
plot(TukeyHSD(model))


## Analiza za�o�e�
#wykres kwantyl-kwantyl
reszty <- lm(y ~ grupy)$residuals
qqnorm(reszty)
qqline(reszty)