data <- c(-0.97,-0.96,-0.93,-0.91,-0.90,-0.87,-0.85,-0.80,-0.75,-0.73,-0.66,-0.65,-0.63,-0.60,-0.58,-0.51,-0.48,-0.45,-0.44,-0.40,-0.37,-0.33,-0.30,-0.25,-0.12,-0.08,0.25,0.41,0.51,0.68)

sturge <- function(x) {
  res = (round(1 + log(length(x)) / log(2)))
  if(res < 5) {
    res = 5
  }
  return(res)
}

nuage <- function() {
  fe <- ecdf(x)
  plot(x, sqrt(1-fe(x)), main="sqrt(1-fe(x))")
}

nuagehxi <- function(x){ #méthode matisse
  lastElem <- length(x)
  myhxi <- (sqrt(1-(seq(1:lastElem)/lastElem)))
  plot(x, myhxi)
}

simTriangle <- function (t) {
  return(t-((2*t)*sqrt(1-runif(1))))
}

displayFixedWidth <- function(x) {
  k=sturge(x)
  print(k)
  a0=-1
  ak=1
  h=0.25
  
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

simTriN <- function(n, t) {
  vect=numeric(n)
  for(i in 1:n) {
    vect[i] = simTriangle(t)
  }
  return(vect)
}

Fx <- function(x) {
  return(1-(((tt-x)^2)/(4*tt^2)))
}

inverse = function (f, lower = -100, upper = 100) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper)[1]
}

main2 <- function() {
  tt <<- 4
  simTriInv <- inverse(Fx, -tt, tt)
  n=3000
  vect=numeric(n)
  for(i in 1:n) {
    vect[i] <- unlist(simTriInv(runif(1))[1], use.names=FALSE)
  }
  
  hist(vect, col=rgb(1,0,0,0.5))
  
  vect=simTriN(3000,tt)
  
  hist(vect, col=rgb(0,1,0,0.5), add=T)
}

main3 <- function(nIt) {
  tSim1 <- 0
  tSim2 <- 0
  tSim3 <- 0
  tSim4 <- 0
  t <- 4
  scoreDispersion1 <- 0
  scoreDispersion2 <- 0
  scoreDispersion3 <- 0
  scoreDispersion4 <- 0
  for(i in 1:nIt){
    vect <- simTriN(300, t)
    tSim1 <- -3*mean(vect) + tSim1
    tSim2 <- max(vect) + tSim2
    tSim3 <- -min(vect) + tSim3
    tSim4 <- (max(vect)-min(vect))/2 + tSim4
    scoreDispersion1 <- scoreDispersion1 + abs(tSim1-t)
    scoreDispersion2 <- scoreDispersion2 + abs(tSim2-t)
    scoreDispersion3 <- scoreDispersion3 + abs(tSim3-t)
    scoreDispersion4 <- scoreDispersion4 + abs(tSim4-t)
  }
  tSim1 <- tSim1/nIt
  cat(tSim1)
  cat("\nbiais = ", ((tSim1/t)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion1)
  tSim2 <- tSim2/nIt
  cat(tSim2)
  cat("\nbiais = ", ((tSim2/t)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion2)
  tSim3 <- tSim3/nIt
  cat(tSim3)
  cat("\nbiais = ", ((tSim3/t)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion3)
  tSim4 <- tSim4/nIt
  cat(tSim4)
  cat("\nbiais = ", ((tSim4/t)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion4)
}