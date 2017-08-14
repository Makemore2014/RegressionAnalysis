estmate <- function(x,y){
  mean.x <- mean(x)
  mean.y <- mean(y)
  sxx <- sum((x-mean.x)^2) #x's variance
  syy <- sum((y-mean.y)^2) #y's variance
  sxy<- sum((y-mean.y)*(x-mean.x)) 
  
  alpha2 <- sxy/sxx
  alpha1 <- mean.y-alpha2*mean.x
  
  n <- length(x)
  sigma <- sqrt(sum((y-alpha1-alpha2*x)^2)/(n-2))
  forecast.y <- alpha1+alpha2*x
  sst <- sum((y-mean.y)^2)
  ssa <- sum((forecast.y-mean.y)^2)
  sse <- sum((y-forecast.y)^2)
  
  R.square <- ssa/sst
  R.square2 <- sxy^2/(sxx*syy)
  return(c(alpha1,alpha2,sigma,R.square,R.square2))
}