#w oryginalnym pliku excela potrzeba usun¹æ cudzys³owia z pierwszego wiersza
data <- read.csv(file = 'weight-height.csv', sep = ',')

length <- nrow(data)

K <- matrix(nrow=5000,ncol=2)
M <- matrix(nrow=5000,ncol=2)

n <- 10 #liczba obserwacji 
m <- n/2 #liczba obserwacji na jedn¹ z dwóch grup

#Podzielnie danych na kobiety i mezczyzn
for (i in 1:length){
  if(data[i, 1] == 'Male'){
    M[i,1] <- data[i,2]
    M[i,2] <- data[i,3]
  }
  else {
    K[i-5000,1] <- data[i,2]
    K[i-5000,2] <- data[i,3]
  }
}

#wybranie ziarna 
set.seed(5352)
#wylosowanie liczb 
a <- ceiling(runif(m)*5000)

dane <- matrix(nrow=n, ncol=2)

for(i in 1:m){
  dane[i,] =+ round(M[a[i],], 0)
  dane[i+m,] =+ round(K[a[i],], 0)
}

#wylosowane dane
print(dane)

#przydzielamy losow dane do dwóch klastróW
klaster1 <- matrix(nrow=n, ncol=2)
klaster2 <- dane

#wybieramy losowe liczby wed³ug ziarna
c <- abs(ceiling(runif(m)*n))

#bedziemy przepisywac do klastrow dane wedlug c
for (i in 1:n){
  for (j in 1:m){
    if(i == c[j]){
      klaster1[i,] =+ dane[i,]
      klaster2[i,] = NA
    }
  }
}

KL1 <- dane
KL2 <- dane
z <- 0 #policzona liczba iteracji

#nastêpne akcje powtarzane do momentu, kiedy nie bêdzie zmian w klastrach
repeat{
  klaster1 <- na.exclude(klaster1) #usuwamy z klastrów NA
  klaster2 <- na.exclude(klaster2)
  print(klaster1)
  print(klaster2)
  
  x1 <- sum(klaster1[,1])/(length(klaster1)/2) #liczymy centroidy pierwszego klastru
  y1 <- sum(klaster1[,2])/(length(klaster1)/2)
  
  x2 <- sum(klaster2[,1])/(length(klaster2)/2) #a nastêpnie drugiego klastru
  y2 <- sum(klaster2[,2])/(length(klaster2)/2)

  #tworzymy wykres z wylosowanymi danymi
  plot(dane, xlab = "Waga[kg]", ylab = "Wzrost[cm]")
  #nastêpnie oznaczamy kolorami poszczególne klastry i zaznaczamy ich centroidy
  points(x1,y1, col = "red", pch = 19)
  points(klaster1, col = "red")
  points(x2,y2, col = "blue", pch = 19)
  points(klaster2, col = "blue")
  
  #zmiana klastrów
  klaster1 = matrix(nrow=n, ncol=2)
  klaster2 = matrix(nrow=n, ncol=2)
  
  #obliczanie odleg³oœci dla punktów od centroidów
  for(i in 1:n){
    print('Klaster 1: ')
    x <- abs(x1-dane[i,1])
    y <- abs(y1-dane[i,2])
    odleglosc1 = sqrt(x*x+y*y)
    print(odleglosc1)
    print('Klaster 2: ')
    x <- abs(x2-dane[i,1])
    y <- abs(y2-dane[i,2])
    odleglosc2 = sqrt(x*x+y*y)
    print(odleglosc2)
    if(odleglosc1<=odleglosc2){
      klaster1[i,] =+ dane[i,]
    }
    else{
      klaster2[i,] =+ dane[i,]
    }
  }
  
  #nowe przepisanie danych do klastrów
  if(length(KL1) == length(na.exclude(klaster1)) &&
     length(KL2) == length(na.exclude(klaster2)) && 
     KL1 == na.exclude(klaster1) && 
     KL2 == na.exclude(klaster2)){break}  
  KL1 <- na.exclude(klaster1)
  KL2 <- na.exclude(klaster2)
  z=z+1
}

#wykorzystanie wbudowany=ego w R algorytmu
klaster = kmeans(dane,2)
str(klaster)
plot(dane, pch=klaster$cluster,  xlab = "Waga[kg]", ylab = "Wzrost[cm]")
points(klaster$centers, pch=19)

#--------------------------------------------------------
#wybór najlepszej iloœci klastrów

library(ggplot2)

tot.withinss <- vector("numeric", length = 10)
for (i in 1:9){
  k_d <- kmeans(dane, i)
  tot.withinss[i] <- k_d$tot.withinss
}

ggplot(as.data.frame(tot.withinss), aes(x = seq(1,10), y = tot.withinss)) +
  theme_minimal() +
  geom_point(col = 'black') +    
  geom_line(col = 'black') +
  ylab("Total Within Sum of Square") +
  xlab("Liczba skupieñ k") +
  ggtitle("Optymalna liczba skupieñ - Metoda Elbow")

klaster = kmeans(dane,5)
str(klaster)
plot(dane, pch=klaster$cluster)
points(klaster$centers, pch=19, col = 'red')