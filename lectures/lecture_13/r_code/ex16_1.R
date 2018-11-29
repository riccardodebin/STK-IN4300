# exercise 16.1 #

library(mvtnorm)

# initialize values
N <- 60
p <- 1000
p_b <- 20
rho = 0.95
noise_to_signal <- 0.72

# construct the covariance matrix
Sigma_b <- matrix(0.95, ncol = p_b, nrow = p_b) + diag(0.05, p_b)

# generate X
X <- NULL
for (i in 1:(p/p_b))
{
  set.seed(i)
  X <- cbind(X, rmvnorm(N, mean = rep(0, p_b), sigma = Sigma_b)) 
}

cor(X[, 1], X[ , 11])
cor(X[, 1], X[ , 21])

# generate the regression coefficients
beta_sign <- rnorm(50)
beta_0 <- rep(0, p)
for (i in 1:(p/p_b))
{
  set.seed(i)
  beta_0[sample((20*(i-1)+1):(20*i), 1)] <- beta_sign[i]
}

# generate Y
signal <- var(X %*% beta_0)
noise <- noise_to_signal*signal
Y <- X %*% beta_0 +rnorm(N, mean = 0, sd = sqrt(noise))



