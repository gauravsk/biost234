#LOAD NECESSARY PACKAGES
library(R2jags)
load("AddBurnin.RData")
library(lattice)

#CHANGE WORKING DIRECTORY
# setwd("C:\\Users\\Rob\\Courses\\Bayes\\labsjags\\lab4")
setwd("~/grad/courses/UCLA/biost234/homework/lab4/")
getwd()

# useful function
mysummary = function(invector) {
c(mean(invector), sd(invector), quantile(invector, .025), 
	quantile(invector,.975),
	length(invector[invector>0])/length(invector))
}

# load the data.  

load("lab4_data.RData")

# inspect the data please.  
# variables are
y
x # Describe each of the columns
str(priordata)   # note:  long!
str(inits)       # even longer.  look at just the first one!

#Create the model

# sink("lab4model.txt")
cat("
model
        {               
                for( i in 1 : 64 ) { # Foreach kid
                        for( j in 1 : 4 ) { # For each measurement
                            s[i, j]<-4*(i-1)+j # Get the row for the current kid at the current time
                            y[i, j] ~ dnorm(mu[i , j],tau.e) # Sample for this kid at this time
                            mu[i , j] <- inprod(x[s[i,j],],alpha[])+beta[i] # sample for the kid
                        }
                        beta[i]~dnorm(0, tau.b) # For each kid, there is a Beta
        }

# Set up priors for alpha
for( k in 1:8) {
                alpha[k]~dnorm(m[k],varinv[k]) # sample a prior for alpha from the distribution
                alphasign[k] <- step(alpha[k]) # Get the sign of alpha (1 for +ve; 0 for -ve)
                }

                tau.e ~ dgamma(ea,eb) # Sample from prior for error
                tau.b~dgamma(ba,bb)   # Sample from prior for beta
                
                # Transform taus to sigmas
                sigma <- 1 /sqrt( tau.e) 
                sqrtD <- 1 /sqrt( tau.b)
                # Rho is something we can look at if we are more interested in variances 
                # than in the estimates of coefficients
                rho <- sqrtD*sqrtD/(sigma*sigma + sqrtD *sqrtD)

        }

    ",fill = TRUE)
# sink()

proc.time()
run1 = jags(priordata, inits, parameters, "lab4model.txt", n.chains=5, n.iter=11000, n.burnin=0, n.thin=1)
proc.time()
# 1100 iterations takes about 3 seconds on my computer.  
# 11000 iterations takes a little under 15 seconds on my computer.  

names(run1)
load("../lab3/AddBurnin.RData")

Output1=AddBurnin(run1$BUGSoutput$sims.array,burnin=1000,n.thin=1)

print(Output1$Burnin.Summary)



#### SWITCH FROM NORMAL TO T-ERROR DISTRIBUTION
# sink("lab4model.T.txt")
cat("
model
        {               
                for( i in 1 : 64 ) { # Foreach kid
                        for( j in 1 : 4 ) { # For each measurement
                            s[i, j]<-4*(i-1)+j # Get the row for the current kid at the current time
                            # y[i, j] ~ dnorm(mu[i , j],tau.e) # Sample for this kid at this time
                              y[i,j] ~ dt( mu[i,j] , tau.e, df1)
                              mu[i , j] <- inprod(x[s[i,j],],alpha[])+beta[i] # sample for the kid
                        }
                        # beta[i]~dnorm(0, tau.b) # For each kid, there is a Beta
                        beta[i]~dt(0, tau.b, df2) # For each kid, there is a Beta
                        

                }

df1 <- 1/invdf1
invdf1 ~ dunif(0,.5)
df2 <- 1/invdf2
invdf2 ~ dunif(0,.5)

# Set up priors for alpha
for( k in 1:8) {
                alpha[k]~dnorm(m[k],varinv[k]) # sample a prior for alpha from the distribution
                alphasign[k] <- step(alpha[k]) # Get the sign of alpha (1 for +ve; 0 for -ve)
}

                tau.e ~ dgamma(ea,eb) # Sample from prior for error
                tau.b~dgamma(ba,bb)   # Sample from prior for beta
                
                # Transform taus to sigmas
                sigma <- 1 /sqrt( tau.e) 
                sqrtD <- 1 /sqrt( tau.b)
                # Rho is something we can look at if we are more interested in variances 
                # than in the estimates of coefficients
                # variance within groups/variance overall? idk How much of the variance explained by group
                rho <- sqrtD*sqrtD/(sigma*sigma + sqrtD *sqrtD)

        }

    ",fill = TRUE)
# sink()


parameters = c("alpha", "alphasign", "tau.e", "tau.b", "sigma", "sqrtD", "rho", "beta[1:5]", "y[4,3:4]", "df1", "df2") 
# run2 = jags(priordata, inits, parameters, "lab4model.T.txt",  n.chains=5, n.iter=11000, n.burnin=0, n.thin=1)
Output2 = AddBurnin(run2$BUGSoutput$sims.array,burnin=100,n.thin=1)

Output2$Burnin.Summary

plot(density(Output1$Burnin.sims.array[,,]))


to_plot = Output2$Burnin.sims.matrix
dim(to_plot)
t(to_plot)
# Plot 1
plot(density((to_plot[,"y[4,3]"])), xlim = c(0, 14), ylim = c(0, 1.2), lwd = 1.5, xlab = "ln(time) (seconds)")
lines(density((to_plot[,"y[4,4]"])), col = "red", lwd = 1.5, lty = 2)
legend("topright", col = c(1,2), legend = c('y[4,3]','y[4,4]'), bty = 'n', lty = c(1,2))

# Plot 2
plot(density(exp(to_plot[,c("alpha[1]")] + 
               to_plot[,c("alpha[2]")])), ylim = c(0, .17), 
     main = "Baseline pain threshold")
lines(density(exp(to_plot[,c("alpha[1]")])), col = "red", lty = 2, lwd = 1.5)
legend("topright", col = c(1,2), legend = c('Distractor','Attender'), bty = 'n', lty = c(1,2), lwd = 2)

# Plot 3
# Treatment effect for attenders
# Log scale
par(mfrow = c(1,2))
plot(density((to_plot[, "alpha[3]"])), main = "Treatment effects on attenders", xlab = "ln(seconds)")
lines(density((to_plot[,"alpha[4]"])), col = "red")
lines(density((to_plot[,"alpha[5]"])), col = "blue")
legend("topleft", col = c("black", "red", "blue"), legend = c("attend", "distract", "null"), lty = 1:3, bty = "n")

plot(density((to_plot[, "alpha[6]"])), main = "Treatment effects on distracters", xlab = "ln(seconds)")
lines(density((to_plot[,"alpha[7]"])), col = "red")
lines(density((to_plot[,"alpha[8]"])), col = "blue")

# Multiplicative scale
par(mfrow = c(1,2))
plot(density(exp(to_plot[, "alpha[3]"])), main = "Treatment effects on attenders", xlab = "seconds", ylim = c(0, 4))
lines(density(exp(to_plot[,"alpha[4]"])), col = "red")
lines(density(exp(to_plot[,"alpha[5]"])), col = "blue")
legend("topright", col = c("black", "red", "blue"), legend = c("attend", "distract", "null"), lty = 1:3, bty = "n")

plot(density(exp(to_plot[, "alpha[6]"])), main = "Treatment effects on distracters", xlab = "seconds", xlim = c(0,2.5))
lines(density(exp(to_plot[,"alpha[7]"])), col = "red")
lines(density(exp(to_plot[,"alpha[8]"])), col = "blue")

# Difference
par(mfrow = c(1,2))
plot(density((to_plot[, "alpha[4]"]-to_plot[,"alpha[3]"])), main = "Treatment effect differences", xlab = "seconds", ylim = c(0, 3))
lines(density((to_plot[,"alpha[4]"]-to_plot[,"alpha[5]"])), col = "red")
lines(density((to_plot[,"alpha[3]"]-to_plot[,"alpha[5]"])), col = "blue")
legend("topright", col = c("black", "red", "blue"), legend = c("D-A", "D-N", "A-N"), lty = 1:3, bty = "n")

plot(density((to_plot[, "alpha[7]"]-to_plot[,"alpha[6]"])), main = "Treatment effect differences", xlab = "seconds", ylim = c(0, 2.5), xlim = c(-1.5, 1.75))
lines(density((to_plot[,"alpha[7]"]-to_plot[,"alpha[8]"])), col = "red")
lines(density((to_plot[,"alpha[6]"]-to_plot[,"alpha[8]"])), col = "blue")

# Question 4
# We suggest the prior $df ~ 2+Gamma(\frac{61^2}{230}, \frac{61}{23]})$. This prior has a minimum value of 2 (due to the 2+ at the beginning), a mean of 63 (which is close to n-1 for our dataset), and a standard deviation of 230 (which is the standard deviation of our first uniform prior). This may be a better prior because it pushes the mean towards our known sample size but still allows for a very wide range of dfs. 

