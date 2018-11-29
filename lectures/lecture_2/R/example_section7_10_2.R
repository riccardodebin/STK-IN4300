install.packages('class')
# use the R routine to perform kNN form the library 'class'
library(class)

# generate training data
set.seed(1)
X <- matrix(rnorm(25e4), ncol = 5000)
y <- rbinom(n = 50, size = 1, prob = 0.5)

# compute the univariate correlation
univariate.correlations <- apply(X, 2, function(x, y) cor(x, y), y = y)
# select the 100 features most correlated with the outcome
best100 <- order(abs(univariate.correlations), decreasing = TRUE)[1:100]
X.red <- X[, best100]

# compute the CV error
mean(y != knn.cv(train = X.red, cl = y, k = 1))
mean(y != knn.cv(train = X, cl = y, k = 1))

# compute the average prediction error over 50 simulations
error.red <- 0
error.full <- 0
for(i in 1:50)
{
  # generate the training data
  set.seed(i)
  X <- matrix(rnorm(25e4), ncol = 5000)
  y <- rbinom(n = 50, size = 1, prob = 0.5)
  
  # select the 100 features most correlated with the outcome
  univariate.correlations <- apply(X, 2, function(x,y) cor(x,y), y=y)
  best100 <- order(abs(univariate.correlations), decreasing = TRUE)[1:100]
  X.red <- X[, best100]
  
  # compute the CV error
  error.red <- error.red + mean(y != knn.cv(train = X.red, cl = y, k = 1))
  error.full <- error.full + mean(y != knn.cv(train = X, cl = y, k = 1))
}

# average over 50 simualtions
error.red / 50
error.full / 50


# CV error with 5-fold cross-validation
cv.error.red <- 0
cv.error <- 0
for(i in 1:50)
{
  # generate the training data
  set.seed(i)
  X <- matrix(rnorm(25e4), ncol = 5000)
  y <- rbinom(n = 50, size = 1, prob = 0.5)

  # select the 100 features most correlated with the outcome
  univariate.correlations <- apply(X, 2, function(x,y) cor(x,y), y=y)
  best100 <- order(abs(univariate.correlations), decreasing = TRUE)[1:100]
  X.red <- X[, best100]
  
  # 5-fold cross-validation
  set.seed(i)
  index <- sample(rep(1:5, 10))
  cv.err <- cv.err.red <- NULL
  for (k in 1:5)
  {
    cv.err.red[k] <- sum(y[index == k] != knn1(train = X.red[index != k, ],
                                                test = X.red[index == k, ],
                                                cl = y[index != k]))
    cv.err[k] <- sum(y[index == k] != knn1(train = X[index != k, ],
                                            test = X[index == k, ],
                                            cl = y[index != k]))
  }
  cv.error.red <- cv.error.red + sum(cv.err.red) / nrow(X)
  cv.error <- cv.error + sum(cv.err) / nrow(X)
}

# average over 50 simulations
cv.error.red / 50
cv.error / 50




###################
### CORRECT WAY ###
###################

# CV error with 5-fold cross-validation
cv.error.red <- 0
for(i in 1:50)
{
  # generate the training data
  set.seed(2 * i)
  X <- matrix(rnorm(25e4), ncol = 5000)
  y <- rbinom(n = 50, size = 1, prob = 0.5)
  
  # 5-fold cross-validation
  set.seed(2 * i)
  index <- sample(rep(1:5, 10))

  cv.err.red <- NULL
  for (k in 1:5)
  {
    # select the 100 features most correlated with the outcome
    univariate.correlations <- apply(X[index != k, ], 2, function(x,y) cor(x,y), y = y[index != k])
    best100 <- order(abs(univariate.correlations), decreasing = TRUE)[1:100]
    X.red <- X[index != k, best100]
    
    cv.err.red[k] <- sum(y[index == k] != knn1(train = X.red,
                                               test = X[index == k, best100],
                                               cl = y[index != k]))
  }
  cv.error.red <- cv.error.red + sum(cv.err.red) / nrow(X)
}

# average over 50 simulations
cv.error.red / 50


