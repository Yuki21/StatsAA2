#x=c(54.8,55.4,57.7,59.6,65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4) # Faux tirage sons
#x=c(54.8,55.4,57.7,59.6,60.1,61.2,62.0,63.1,63.5,64.2,65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4) # Vrai tirage sons
#x=c(0,26,78,130,182,234,275) # Les ampoules
#x<-c(91.6, 35.7, 251.3, 24.3, 5.4, 67.3, 170.9, 9.5, 118.4, 57.1) # Les vrais ampoules
x <- c(1.0,2.1,2.8,3.7,5.3,6.0,6.5,7.2,8.2,9.5)

displayFixedWidth <- function(x) {
  k=sturge(x)
  print(k)
  a0=min(x)-0.025*(max(x)-min(x))
  ak=max(x)+0.025*(max(x)-min(x))
  h=(ak-a0)/k
  
  hist = hist(x, prob=T, breaks=seq(a0,ak,h), xlim=c(a0,ak), main="Histogramme à largeur fixée")
  lines(hist$mids, hist$density, lwd=3, col="Red")
  lines(density(x), lwd=3, col="Blue")
  plot(ecdf(x), main="Fonction de répartition empirique pour largeur fixée")
}

displayFixedEffec <- function(x) {
  x = sort(x)
  k=sturge(x)
  h = length(x)/k
  i=1
  j=1
  res <- numeric(length = k+1)
  while(i<=length(x)) {
    res[j] = x[i]
    i = i+h
    j=j+1
  }
  res[k+1]=max(x)
  
  print(res)
  hist = hist(x, prob=T, breaks=res, main="Histogramme à effectif fixé")
  lines(hist$mids, hist$density, lwd=3, col="Red")
  lines(density(x), lwd=3, col="Blue")
  plot(ecdf(x), main="Fonction de répartition empirique pour effectif fixé")
}

expQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e],log(1-seq(1:e)/(e+1)),ylim=c(-2.5,0.1), main="Q-Q Plot for exp law")
  abline(v=0)
  abline(h=0)
}

normQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qnorm(seq(1:e)/(e+1)), main="Q-Q Plot for normal law")
  abline(h=0)
}

uniQQplot <- function(x) {
  e=length(x)
  plot(sort(x)[1:e], qunif(seq(1:e)/(e+1)), main="Q-Q Plot for uni law")
  abline(v=0)
  abline(h=0)
}

sturge <- function(x) {
  res = (round(1 + log(length(x)) / log(2)))
  if(res < 5) {
    res = 5
  }
  return(res)
}

exo1 <- function() {
  x <- runif(1000) # Tirage simulé uni
  displayFixedWidth(x)
  displayFixedEffec(x)
}

exo2 <- function() {
  expData <- rexp(1000, rate=.1) # Tirage simulé exp
  normData <- rnorm(1000) # Tirage simulé norm
  uniData <- runif(1000) # Tirage simulé uni
  expQQplot(x)
  normQQplot(x)
  #qqnorm(normData) # C'est presque le même truc en vrai
  uniQQplot(x)
}

app <- function() {
  # uniQQplot(x)
  # expQQplot(x)
  # normQQplot(x)
  # 
  
  resM=0
  resJ=0
  moyM=0
  moyJ=0
  b=15
  for(i in 1:1000) {
    x1 = runif(10,0,b)
    marius=(sum(x1)/length(x1))*2
    jeannette=max(x1)
    moyM=moyM+marius
    moyJ=moyJ+jeannette
    resM=resM+abs(b-marius)
    resJ=resJ+abs(b-jeannette)
  }
  print(resJ)
  print(resM)
  print(moyJ/1000)
  print(moyM/1000)
}