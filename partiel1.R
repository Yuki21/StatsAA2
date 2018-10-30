x <- c(0.22,1.64,5.06,0.24,1.96,6.41,0.29,2.27,7.58,0.29,2.44,7.81,0.33,2.75,8.00,0.47,2.99,8.24,0.85,3.15,10.15,1.14,3.85,12.24,1.50,4.66,13.78,1.51,5.04,16.12)

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

fAllQQPlot <- function(x){
  expQQplot(x)
  normQQplot(x)
  uniQQplot(x)
}

#Moyenne empirique
moy <- mean(x)
#
lambda = 1/moy

simuExp <- function(nIt){
  lambdaSim <- 0
  lambda <- 10
  scoreDispersion <- 0
  for(i in 1:nIt){
    vect <- rexp(30, lambda)
    lambdaSim <- 1/mean(vect) + lambdaSim
    scoreDispersion <- scoreDispersion + abs(lambdaSim-lambda)
  }
  lambdaSim <- lambdaSim/nIt
  cat(lambdaSim)
  cat("\nbiais = ", ((lambdaSim/lambda)*100)-100, "%")
  cat("\nScore dispersion = ", scoreDispersion)
}

simuExp(10000)

#lambdaSim != lambda donc biais
#calculBiais = (LambdaSim/LambdaRExp)*100
#calculConvergence -> FAIRE UNE SEULE SIMU

simuExp(1)

