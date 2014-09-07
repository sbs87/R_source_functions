# An implementation of the Bayesian LASSO, based on page
# 400 of *Penalized Regression, Standard Errors, and Bayesian Lassos*.
#
# DMD, 111113-21-45

library(mvtnorm) # For sampling from a multivariate normal distribution.
library(statmod) # For sampling from an inverse Gaussian distribution.

p = 10
n = 1000

# beta = matrix(1, ncol = 1, nrow = p)

# beta = matrix(seq(1, p, by = 1), ncol = 1, nrow = p)

beta = matrix(c(rep(0, p/2), rep(1, p/2)), ncol = 1, nrow = p)

X = matrix(rnorm(n*p), nrow = n, ncol = p)

sigma.true = 2

lambda = 1

Y = X%*%beta + sigma.true*rnorm(n)

gram = t(X)%*%X

Y.tilde = Y - mean(Y)

num.burn.in = 1000
num.estimation = 5000

num.samples = num.burn.in + num.estimation

betas = matrix(0, nrow = p, ncol = num.samples)

recip.tau.squares = matrix(0, nrow = p, ncol = num.samples)

sigma.squares = matrix(1, nrow = 1, ncol = num.samples)

for (sample.ind in 2:num.samples){
  D = diag(sqrt(recip.tau.squares[, sample.ind - 1]))
  
  M = solve(gram + D)
  
  betas[, sample.ind] = rmvnorm(n = 1, mean = M%*%t(X)%*%Y.tilde, sigma=sigma.squares[sample.ind - 1]*M)
  
  for (j in 1:p){
    recip.tau.squares[j, sample.ind] = rinvgauss(n = 1, mu = lambda^2*sqrt(sigma.squares[sample.ind]) / abs(betas[j, sample.ind]), lambda^2)
  }
  
  D = diag(sqrt(recip.tau.squares[, sample.ind]))
  
  Y.shifted = Y.tilde - X%*%betas[, sample.ind]
  
  sigma.squares[sample.ind] = 1/rgamma(n = 1, shape = 0.5*(n - 1 + p), rate = 0.5*t(Y.shifted)%*%Y.shifted + 0.5*lambda*t(betas[, sample.ind])%*%D%*%betas[, sample.ind])
}

betas.use = betas[, (num.burn.in+1):(num.burn.in + num.estimation)]

# The posterior density of the betas.

for (j in 1:p){
  plot(density(betas.use[j, ]), main = paste0('j = ', j))
}

# The posterior density of the standard deviation.

plot(density(sqrt(sigma.squares)))

# The posterior means for the betas.

rowMeans(betas.use)
