{
y = seq(0,2,0.1)
theta = 1.2
posteriormean = theta*(exp(2*theta*y)-1)/(exp(2*theta*y)+1)
priormean = y
plot(y,priormean,type='l')
lines(y,posteriormean)
}

combination <- function(n,x){
  return(factorial(n)/(factorial(x)*factorial(n-x)))
}

betatail <- function(a,b,quantile){
  return(quantile(rbeta(10000,a,b),probs = quantile))
}

HDP <- function(theta,n,a,b,alpha){
  input = 0:n
  
  thetalow = mapply(betatail, input + a, n - input + b, quantile = alpha/2)
  thetaup = mapply(betatail, input + a, n - input + b, quantile = 1- alpha/2)
  
  lower = (theta<thetalow)*combination(n,input)*(theta^input)*((1-theta)^(n-input))
  upper = (theta>thetaup)*combination(n,input)*(theta^input)*((1-theta)^(n-input))
  coverage = c(sum(lower),1-sum(upper)-sum(lower))
  return(coverage)
}

{
  #Figure4.6
  a = c(0.5,1,5)
  theta = seq(0,1,0.05)
  cover = theta
  for(i in 1:3){
    temp = t(mapply(HDP,theta,10,a[i],a[i],alpha))
    cover = cbind(cover,temp)
  }
  plot(theta,cover[,7],type = 'l',col="blue")
  for(i in 3:7){
    lines(theta,cover[,i])
  }
}
