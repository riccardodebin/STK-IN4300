rm(list=ls())
library(rpart)

### point (a) ###

set.seed(1)
X <- matrix(rnorm(20000), ncol = 10)
y <- apply(X^2, 1, sum)
y[y <= qchisq(p = 0.5, df = 10) ] <- -1
y[y > qchisq(p = 0.5, df = 10) ] <- 1
table(y)
N <- length(y)
B <- 100

# 1: initialization
f <- list()
w <- rep(1/N, N)
b  <- matrix(NA, ncol = N, nrow = B)
err <- alpha <- NULL
# 2: iterations
for (m in 1:B)
{
  f[[m]] <- rpart(as.factor(y)~., data = as.data.frame(X), weights = w, control = rpart.control(maxdepth = 1))
  b[m, ] <- 2*(as.numeric(predict(f[[m]], newdata = as.data.frame(X), type = 'class')) - 1.5)
  err[m] <- w %*% (b[m,] != y) / sum(w)
  alpha[m] <- log((1 - err[m]) / err[m])
  w <- w * exp(alpha[m] * as.numeric(b[m, ] != y))
  # w <- w/sum(w)
  print(m)
}
# 3: final classifier
g <- rep(0, N)
for (m in 1:B) g <- g + alpha[m] * b[m, ]
table(g>0)
sum((g>0)!=(y>0))

### point (b) ###

# wrt boosting iterations
train.error<-NULL
for (i in 1:B)
{
  g <- rep(0, N)
  for (m in 1:i) g <- g + alpha[m] * b[m, ]
  train.error[i] <- sum((g > 0) != (y > 0)) / N
}
plot(1:B, train.error, type = 'l')

# test error
set.seed(1)
X.test <- matrix(rnorm(100000), ncol = 10)
y.test <- apply(X.test^2, 1, sum)
y.test[y.test <= qchisq(p = 0.5, df = 10) ] <- -1
y.test[y.test > qchisq(p = 0.5, df = 10) ] <- 1
table(y.test)
N.test <- length(y.test)

test.error <- NULL
b.test  <- matrix(NA, ncol = N.test, nrow = B)
g.test <- rep(0, N)
for (m in 1:B)
{
  b.test[m,] <- 2*(as.numeric(predict(f[[m]], newdata = as.data.frame(X.test), type = 'class')) - 1.5)
  g.test <- g.test + alpha[m] * b.test[m, ]
  test.error[m] <- sum((g.test > 0) != (y.test > 0)) / N.test
}  
plot(1:B, test.error, type = 'l')

### point(c) ###
B <- 1000
# rerun with B = 1000
which.min(test.error)


### point (d) ###

set.seed(1)
X <- matrix(rnorm(20000), ncol = 10)
y <- apply(X^2, 1, sum)
y[y <= 12 ] <- -1
y[y > 12 ] <- 1
table(y)
N <- length(y)
B <- 500

# 1: initialization
f <- list()
w <- rep(1/N, N)
b  <- matrix(NA, ncol = N, nrow = B)
err <- alpha <- NULL
# 2: iterations
for (m in 1:B)
{
  f[[m]] <- rpart(as.factor(y)~., data = as.data.frame(X), weights = w, control = rpart.control(maxdepth = 1))
  b[m,] <- 2*(as.numeric(predict(f[[m]], newdata = as.data.frame(X), type = 'class')) - 1.5)
  err[m] <- w %*% (b[m,] != y) / sum(w)
  alpha[m] <- log((1 - err[m]) / err[m])
  w <- w * exp(alpha[m] * as.numeric(b[m, ] != y))
  # w <- w/sum(w)
  print(m)
}
# 3: final classifier
g <- rep(0, N)
for (m in 1:B) g <- g + alpha[m] * b[m, ]
table(g > 0)
sum((g > 0) != (y > 0))

# wrt boosting iterations
train.error <- NULL
for (i in 1:B)
{
  g <- rep(0, N)
  for (m in 1:i) g <- g + alpha[m] * b[m, ]
  train.error[i] <- sum((g > 0) != (y > 0)) / N
}
plot(1:B, train.error, type = 'l')


# test error
set.seed(1)
X.test <- matrix(rnorm(100000), ncol = 10)
y.test <- apply(X.test^2, 1, sum)
y.test[y.test <= 12 ] <- -1
y.test[y.test > 12 ] <- 1
table(y.test)
N.test <- length(y.test)

test.error <- NULL
b.test  <- matrix(NA, ncol = N.test, nrow = B)
g.test <- rep(0, N.test)
for (m in 1:B)
{
  b.test[m,] <- 2*(as.numeric(predict(f[[m]], newdata = as.data.frame(X.test), type = 'class')) - 1.5)
  g.test <- g.test + alpha[m] * b.test[m, ]
  test.error[m] <- sum((g.test > 0) != (y.test > 0)) / N.test
}  
plot(1:B, test.error, type = 'l')

which.min(test.error)
