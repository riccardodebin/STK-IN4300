# extra exercise lecture 12 #

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
beta_sign <- rnorm(p/p_b)
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


### apply supervised principal component ###
library(pls)

# step 1: order the variables
scores <- apply(X, 2, function(x, y) abs(cor(x, y)), y = Y)
thresholds <- sort(scores)

# step 2: perform principal components
X.std <- scale(X)
for (i in 1:p)
{
  X.sel <- X[ , scores >= thresholds[i]]
  mod.pcr <- pcr(Y ~ ., data = as.data.frame(X.sel),
                 validation = 'none')
  summary(mod.pcr)
}

# step 3: select \theta and m
set.seed(1)
index <- sample(rep(1:6, N/6), N, replace = FALSE)
rmsep <- array(NA, dim = c(p, 45, 6)) # 45 = max number pc, 5 = number of folds
for (k in 1:6)
{
  X.train <- X[index != k, ]
  X.train.mean <- apply(X.train, 2, mean)
  X.train.sd <- apply(X.train, 2, sd)
  X.train.std <- scale(X.train)
  X.test <- sapply(1:p, function(i, XX, m, s) (XX[ ,i] - m[i])/s[i],
                    XX = X[index == k, ], m = X.train.mean, s = X.train.sd)
  # super PC for each fold
#  scores <- apply(X.train, 2, function(x, y) abs(cor(x,y)), y = Y[index != k])
#  thresholds <- sort(scores)
  for (i in 1:p)
  {
    X.sel <- X.train[ , scores >= thresholds[i]]
    mod.pcr <- pcr(Y[index != k] ~ ., data = as.data.frame(X.sel),
                   validation = 'CV')
    rmsep[i, 1:min(45, 1002 - i), k] <- RMSEP(mod.pcr)$val[2, , ]
  }
}

error <- apply(rmsep, c(1, 2), sum)
tuning_param <- arrayInd(which.min(error), dim(error))
theta_sel <- thresholds[tuning_param[1]]
m_sel <- tuning_param[2] - 1

# fit final model
X.final <- X[ , scores >= theta_sel]
mod.pcr <- pcr(Y ~ ., data = as.data.frame(X.final),
               validation = 'none', segments = m_sel)
summary(mod.pcr)
