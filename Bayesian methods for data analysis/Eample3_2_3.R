#Example 3.2.3
#The data are discret and the Poisson likelihood 
#with individual acccident rates drawn from a prior 
#distribution is a good candidate model
Yi <- seq(0,7,1)
Count <- c(7840,1317,239,42,14,4,4,1)
accident <- data.frame(Yi,Count)
Margindist = accident$Count/sum(accident$Count)
round(Margindist,5)
accident = cbind(accident,Margindist)

Robbins = accident$Yi
for(i in 0:7){
  Robbins[i+1] = round((i+1)*accident$Count[i+2]/accident$Count[i+1],3)
}
accident = cbind(accident,Robbins)

Ymean = sum(accident$Yi * accident$Margindist) #=alpha/beta
Yvariance = sum(accident$Yi * accident$Margindist * accident$Yi) - Ymean^2
beta = (Yvariance - Ymean)/Ymean
weightonprior = 1/(1+beta)

Gamma = 0:7
for(i in 0:7){
Gamma[i+1] = round(Ymean weightonprior + i * (1 - weightonprior),3)
}
accident = cbind(accident,Gamma)
