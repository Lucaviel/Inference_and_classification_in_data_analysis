library(graphics)
library(qtl)

#wylosowanie 15 nowych wartoœci

set.seed(27042021)
rnorm(15,10.2, 3.8)

A = read.csv('C:\\Users\\Laptop\\Desktop\\dane13.csv',header = TRUE, sep = ",")

#rozdzielam dane wg klasy

y <- A[,1]
grupy <- factor(A[,2])

A1={}
A2={}
A3={}
A4={}

for(i in 1:55){
  if(A[i,2]==1){A1[i]=A[i,1]}
  if(A[i,2]==2){A2[i]=+A[i,1]}
  if(A[i,2]==3){A3[i]=+A[i,1]}
  if(A[i,2]==4){A4[i]=+A[i,1]}
}
A1=na.omit(A1) 
A2=na.omit(A2) 
A3=na.omit(A3) 
A4=na.omit(A4) 

#minimum, 1 kwartyl, mediana, œrednia, trzeci kwartyl, maksimum

summary(A1)
summary(A2)
summary(A3)
summary(A4)

#wariancja

vars <- tapply(y, grupy, var)

#wykres pudelkowy
plot(grupy,y)

#tabelka
mu<-mean(y)
mui<-tapply(y,grupy,mean)
alpha<-mui - mu

k = 4
n = 55

SST <- sum((y - mu)^2)
SSW <- sum((y[grupy==1] - mui[1])^2)+sum((y[grupy==2] - mui[2])^2)+sum((y[grupy==3] - mui[3])^2)+sum((y[grupy==4] - mui[4])^2)
SSB <- 14*alpha[1]^2+12*alpha[2]^2+14*alpha[3]^2+15*alpha[4]^2

Fu = (SSB/(k-1))/(SSW/(n-k))
  
t= c('zródlo zmiennosci', 'liczba stopni swobody', 'suma kwadratów (SS)','srednia kwadratów', 'wartosc statystyki F')
t1=c('miedzy grupami',k-1,SSB, SSB/(k-1),Fu)
t2=c('wewnatrz grup',n-k,SSW, SSW/(n-k),'-')
t3=c('razem', n-1,SST, '-','-')
rbind(t,t1,t2,t3)

#Poniewaz srednie w poszczególnych grupach nie sa sobie równe, wykonujemy testy post-hoc

anova(lm(y ~ grupy))
model<-aov(y ~grupy)
summary(model)
  #A zatem na poziomie istotnosci ?? = 0.05 nie mozemy odrzucic hipotezy zerowej o równosci srednich

TukeyHSD(model, "grupy")
plot(TukeyHSD(model, "grupy"))

#zalozenia ANOVA

  #rozklad wyników w kazdej grupie ma rozklad zblizony do normalnego
  g <- lm(y ~ grupy)
  qqplot(g$fitted,g$res,pch=16,main='log')
  qqnorm(g$res)
  qqline(g$res)
    #wniosek:rozklad jest zblizony do rozkladu normalnego

  #wariancje w grupach sa jednorodne
  
  bartlett.test(y,grupy)
    #p-value jest wieksze od 0.05, zatem nie ma podstaw, by odrzucic hipoteze zerowa o równosci wariancji w grupach 
  #grupy nie sa równoliczne, wiec ANOVA nie jest odporna na brak równosci wariancji i normalnosci rozkladu  
