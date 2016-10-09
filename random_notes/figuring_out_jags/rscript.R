N <- 1000
x <- rnorm(N, 0, 5)
 
write.table(x,
            file = 'example1.data',
            row.names = FALSE,
            col.names = FALSE)


sink("dummy_model.txt")
cat("model {
	for (i in 1:N) {
		x[i] ~ dnorm(mu, tau)
	}
	mu ~ dnorm(0, .0001) # change this to dnorm(prior_mean and 1/prior_sd^2)
	tau <- pow(sigma, -2) # tau equal to 1/sigma^2
	sigma ~ dunif(0, 100) # change this to be sampling_sd
}", fill = TRUE)
sink()

library('rjags')
library("R2jags")
 
jags <- jags.model('dummy_model.txt',
                   data = list('x' = x,
                               'N' = N), # add prior_sd, prior_mean, sampling_sd
                   n.chains = 4,
                   n.adapt = 100)

jags.samples(jags,
             c('mu', 'tau'),
             1000)