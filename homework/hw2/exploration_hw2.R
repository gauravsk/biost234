library(R2jags)
al1 = 49/9
be1= 7/9
gg <- rgamma(1000, shape = al1, rate = be1)
plot(density(gg))
mean(gg)
sd(gg)


ys <- c(3,6,1,0,9)

al1p = (al1+sum(ys))
be1p = be1+length(ys)

post_mean = (al1p)/be1p
post_mean
ggpost <- rgamma(1000, shape = al1p, rate = be1p)
lines(density(ggpost))
plot(density(ggpost), xlim = c(0, 25))
lines(density(gg), lty = 2, col = "blue")


### Write the JAGS model that will do this shite

# sink("model_hw2.txt")
cat("model {
  # prior
  lambda ~ dgamma(alpha, beta)
  
  for (i in 1:N){
    x[i] ~ dpois(lambda)
  }


}", fill = TRUE)
# sink()

jags.params = c("lambda")

# data
x = ys
N = length(ys)
alpha = al1
beta = be1
jags.data = list("x", "N", "alpha", "beta") 

# initials
jags.inits = function(){
    list("lambda" = 1) # part of algorithm!
}

hw2.sim = jags(jags.data, jags.inits, jags.params, 
              model.file = "test_model_hw2.txt", 
              n.chains = 1, n.iter = 11000, n.burnin = 1000)

lines(density(hw2.sim$BUGSoutput$sims.array[,,2]), col = "red")
  