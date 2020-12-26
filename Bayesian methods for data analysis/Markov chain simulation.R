set.seed(38)

n = 1500
x = numeric(n)
phi = -0.6

for (i in 2:n) {
  x[i] = rnorm(1, mean=phi*x[i-1], sd=1.0)
}

plot.ts(x)

hist(x,freq = FALSE)
curve(dnorm(x,mean=0,sd=sqrt(1/(1-phi^2))),col="red",add=TRUE)

sta = matrix(c(0,0.3,1,0.7),ncol = 2)

#Markov chain simulation
#start at either coin is fair or loaded
#Then for i=1,..,m,propose candidate theta* to be the other state as thetai-1
#choose g(theta) to be joint distribution, q(theta) to be 1
#then alpha = g(theta*)*/g(thetai-1)


