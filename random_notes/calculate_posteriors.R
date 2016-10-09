## Calculating the posterior given priors and data
## Assumes that everything is normal-normal distributed

prior_mean = 200
prior_sd   = 500
sampling_sd = 3
n = 1
ybar = 203

(n/(sampling_sd^2))/((n/sampling_sd^2)+(1/prior_sd^2))*ybar +
  (1/prior_sd^2)/((n/sampling_sd^2)+(1/prior_sd^2)) * prior_mean

