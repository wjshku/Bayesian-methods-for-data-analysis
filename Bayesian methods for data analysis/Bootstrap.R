realmean = 5
realsd = 5
times = 1000
realsample = rnorm(10,realmean,realsd)
fakesample = matrix(0,times,6)
set.seed(30)
for(i in 1:times){
  for(j in 1:5){
    num = sample(1:10,1)
    fakesample[i,j] = realsample[num]
  }
  fakesample[i,6] = mean(fakesample[i,1:5])
}
mean(realsample)
sd(realsample)
mean(fakesample[,6])
sd(fakesample[,6])
plot(fakesample[,6],type='l')