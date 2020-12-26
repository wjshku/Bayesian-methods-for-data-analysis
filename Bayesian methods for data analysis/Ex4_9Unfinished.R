gammatail <- function(a,b,quantile){
  return(quantile(rgamma(10000,a,b),probs = quantile))
}

