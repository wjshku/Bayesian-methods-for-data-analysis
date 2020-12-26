
times = 300
theta = rep(0,times)
x = rep(500,times + 1)
for(i in 1:times){
#Imputation Step 
#Draw theta from Beta(v1,v2)
theta[i] = rbeta(1,x[i]+35,39)

#Draw X from Bino(125,theta/(theta+2))
x[i+1] = rbinom(1,125,theta[i]/(theta[i] + 1))

#Posterior Step
#Update v1 = x + 34 + 1, v2 = 1 + 18 + 20
}

plot(theta,type='l',col='blue')
lines(theta,col="red")
hist(theta[10:times],probability = TRUE,col="blue")

Gibbs <- function(times,initial,data){
  alpha = initial[11]
  result = matrix(0,times+1,12)
  c = 0.1
  d = 1
  result[1,] = initial
  for(i in 1:times){
    for(j in 1:10){
      result[i+1,j] = mean(rgamma(10,shape = data$y[j]+result[i,11],scale = 1/(data$t[j] + 1/result[i,12])))
    }
    result[i+1,11] = alpha
    result[i+1,12] = mean(1/rgamma(10,shape = 10*alpha+c,scale = 1/(sum(result[i,1:10])+1/d)))
  }
  return(result)
}

#Example 5.5
{
initial = c(1,1,1,1,1,1,1,1,1,1,0.7,1)
y = c(5,1,5,14,3,19,1,1,4,22)
t = c(94.32,15.72,62.88,125.76,5.24,31.44,1.048,1.048,2.096,10.48)
data = data.frame(y,t)
result = Gibbs(1000,initial,data)
summary(result)
for(i in 1:12){
  print(sd(result[,i])) #Note that although theta5 and theta6 has similarly mean. Their sd are quite different. This is because that t6 > t5, which intuitivly means that we "know more about pump6"
}
plot(result[,4],type ='l',col='blue',ylim = c(0,1))}

#Example 6.5

