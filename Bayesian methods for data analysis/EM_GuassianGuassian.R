EMsample <- function(i,mean,var){ #This is used to generate the really sample
  theta = 1:i
  y = 1:i
  sigma = 1
  for(j in 1:i){
    theta[j] = rnorm(1,mean,sqrt(var))
    y[j] = rnorm(1,theta[j],sigma)
  }
  return(data.frame(y,theta))
}

EMalgo <- function(times,mean0,var0,data){
  hypermean = 1:(times+1)
  hypervar = 1:(times+1)
  hypermean[1] = mean0
  hypervar[1] = var0
  for(i in 1:times){
    thetaexpect = (hypermean[i] + hypervar[i]*t(data$y))/(1 + hypervar[i]) #This calculate the Expectation of Thetai
    thetasquareexpect = thetaexpect^2 + hypervar[i]/(1+hypervar[i]) #This calculate the Expectation of Thetai square

    hypermean[i+1] = mean(thetaexpect) #This formula comes from maximizing the Expectation of log likelihood function f(x;theta) conditional on x since we do not know the true value of thetai, we can only approximate it from conditional expectation
    hypervar[i+1] = mean(thetasquareexpect) - hypermean[i+1]^2
  }
  return(round(data.frame(hypermean,hypervar),3))
}

{
  size = 50
  realhypermean = 10
  realhypervar = 5

  sample = EMsample(size,realhypermean,realhypervar)
  
  initialmean = 0
  initialvar = 1
  times = 10
  EMresult = EMalgo(times,initialmean,initialvar,sample)
  plot(EMresult$hypermean,type='l',col='blue',ylim = c(0,max(EMresult)*1.1))
  lines(EMresult$hypervar,type='l',col='red')
  abline(realhypermean,0,col='blue')
  abline(realhypervar,0,col='red')
  summary(EMresult)
}


