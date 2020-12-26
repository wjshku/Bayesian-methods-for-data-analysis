fishersleep = c(1.2,2.4,1.3,1.3,0,1,1.8,0.8,4.6,1.4)
k = 10
miuhat = mean(fishersleep)
gammasquarehat = var(fishersleep) - 1

normaltail <- function(mean,var,quantile){
  return(quantile(rnorm(100000,mean,var),probs = quantile))
}

naiveEBCI <- function(mean,var,alpha){
  return(c(mean + normaltail(0,1,alpha)*sqrt(var),mean - normaltail(0,1,alpha)*sqrt(var)))
}
