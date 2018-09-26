#x=c(54.8,55.4,57.7,59.6,65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4)
x=c(54.8,55.4,57.7,59.6,60.1,61.2,62.0,63.1,63.5,64.2,65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4)
displayFixedWidth <- function(x) {
  k=sturge(x)
  a0=min(x)-0.025*(max(x)-min(x))
  ak=max(x)+0.025*(max(x)-min(x))
  h=(ak-a0)/k
  
  hist(x, prob=T, breaks=seq(a0,ak,h), xlim=c(a0-4,ak+2))
}

displayFixedEffec <- function(x) {
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
  hist(x, prob=T, breaks=res)
}

sturge <- function(x) {
  return(floor(1 + log(length(x)) / log(2)))
}

displayFixedWidth(x)
displayFixedEffec(x)