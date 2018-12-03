# load the package ElemStatLearn which contain the prostate data
library(ElemStatLearn)
# load the prostate data
data("prostate")
str(prostate)
y <- prostate$lpsa[prostate$train == TRUE]
X <- as.matrix(prostate[prostate$train == TRUE, -c(9,10)])
summary(X)
# for pure prediction
X <- scale (X)
# note that, if one wants to maintain "interpretability", the standardization of binary variables is not
# advantageous. Therefore:
# X[, -5] <- scale(X[, -5])
# Other choices include the codification -1, 1, s.t., for p(X==1)=0.5, E[X]=0, Var(X)=1, i.e.,
# X[X[, 5] == 0, 5] <- -1
#
# add intercept
X <- cbind(rep(1, nrow(X)), X)


### least square estimates ###
beta.LS <- solve(t(X) %*% X) %*% t(X) %*% y
round(beta.LS, 3)
# NB: in order to obtain the values of Table 3.3, one should first standardize and then
# select the training set. Do not do that in practice!
X.book <- as.matrix(prostate[, -c(9,10)])
X.book <- scale(X.book)
X.book <- cbind(rep(1, sum(prostate$train == TRUE)), X.book[prostate$train == TRUE, ])
beta.LS.book <- solve(t(X.book) %*% X.book) %*% t(X.book) %*% y
round(beta.LS.book, 3)


### best subset ###
# since the R routines include the intercept by default, remove the first column of X
X <- X[, -1]

### all subset ###
allCombinations <- sapply(1:8, function(m) combn(x = 1:8, m = m))

null.model<-lm(y~1)
result.AIC <- extractAIC(null.model)
result.RSS <- cbind(1, deviance(null.model))
for (i in 1:8) # number of combinations
{
  for (j in 1:ncol(allCombinations[[i]]))
  {
    model<-lm(y ~ ., data = as.data.frame(X[, allCombinations[[i]][, j]]))
    result.AIC <- rbind(result.AIC, extractAIC(model))
    result.RSS <- rbind(result.RSS, cbind(length(allCombinations[[i]][, j]) + 1, deviance(model)))
  }
}

# reproduce Figure 3.5
plot(result.RSS[, 1] - 1, result.RSS[, 2], pch = 16, main = 'Figure 3.5', ylim = c(0, 100), 
     xlab = 'Subset size', ylab = 'Residual sum of squares')

# identify the model with the smallest AIC
best.AIC<-result.AIC[which.min(result.AIC[,2]), ]
index<-which.min(result.AIC[result.AIC[,1]==best.AIC[1],2])
variables.best.model <- allCombinations[[best.AIC[1] - 1]][, index]

# final model
model.bestSubset.AIC <- lm(y ~ ., data = as.data.frame(X[, variables.best.model]))
summary(model.bestSubset.AIC)

# we can reply the computations with the book's standardization
result.AIC <- extractAIC(null.model)
result.RSS <- cbind(1, deviance(null.model))
X.book <- X.book[, -1]
for (i in 1:8)
{
  for (j in 1:ncol(allCombinations[[i]]))
  {
    model<-lm(y ~ ., data = as.data.frame(X.book[, allCombinations[[i]][, j]]))
    result.AIC <- rbind(result.AIC, extractAIC(model))
    result.RSS <- rbind(result.RSS, cbind(length(allCombinations[[i]][, j]) + 1, deviance(model)))
  }
}
plot(result.RSS[, 1] - 1, result.RSS[, 2], pch = 16, main = 'Figure 3.5', ylim = c(0, 100), 
     xlab = 'Subset size', ylab = 'Residual sum of squares')
best.AIC<-result.AIC[which.min(result.AIC[,2]), ]
index<-which.min(result.AIC[result.AIC[,1]==best.AIC[1],2])
variables.best.model <- allCombinations[[best.AIC[1] - 1]][, index]
model.bestSubset.AIC.book <- lm(y ~ ., data = as.data.frame(X.book[, variables.best.model]))
summary(model.bestSubset.AIC.book)

# backward elimination #
model.1 <- lm(y ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data = as.data.frame(X))
summary(model.1)
model.2 <- lm(y ~ lcavol + lweight + age + lbph + svi + lcp + pgg45, data = as.data.frame(X))
summary(model.2)
model.3 <- lm(y ~ lcavol + lweight + lbph + svi + lcp + pgg45, data = as.data.frame(X))
summary(model.3)
model.4 <- lm(y ~ lcavol + lweight + lbph + svi + pgg45, data = as.data.frame(X))
summary(model.4)
model.5 <- lm(y ~ lcavol + lweight + lbph + svi, data = as.data.frame(X))
summary(model.5)
model.6 <- lm(y ~ lcavol + lweight + svi, data = as.data.frame(X))
summary(model.6)
model.7 <- lm(y ~ lcavol + lweight, data = as.data.frame(X))
summary(model.7)
model.7$coefficients

model.7.book <- lm(y ~ lcavol + lweight, data = as.data.frame(X.book))
round(model.7.book$coefficients, 3)

# stepback selection #
add1(object = model.7, scope = model.1) # NB: it whould be done at each step, here only at the last
# variable which decrease the least the RSS is svi (see EX 3.10 to understand why we choose it)
model.8 <- lm(y ~ lcavol + lweight + svi, data = as.data.frame(X))
summary(model.8) # we keep model 7


### ridge regression ###
# we first need to find the value of lambda: here by cross-validation
set.seed(1)
# assign the observation randomply to 5 folds
N <- length(y)
indexes <- sample(N)
fold <- list()
fold[[1]]<-indexes[1:14]
fold[[2]]<-indexes[15:28]
fold[[3]]<-indexes[29:41]
fold[[4]]<-indexes[42:54]
fold[[5]]<-indexes[55:67]

# we work without intercept
y <- prostate$lpsa[prostate$train == TRUE]
y.mean <- mean(y)
y <- y - y.mean

# define the ridge estimator
beta.ridge.estimator <- function(X, y, lambda) {
  solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
}
# define possible values of lambda
lambda.seq <- seq(from = 1, to = 10, length.out = 20)
# compute the square errors
square.errors <- matrix(0, ncol= length(lambda.seq), nrow = 5)
for (i in 1:5) {
  for (j in 1:length(lambda.seq)) {
    X.train <- X[-fold[[i]], ]
    X.train.mean <- apply(X.train, 2, mean)
    X.train.sd <- apply(X.train, 2, sd)
    X.train <- scale (X.train, center = TRUE, scale = TRUE)
    X.test <- X[fold[[i]], ]
    X.test <- sapply(1:ncol(X.test),
                     function(k, X.test, X.mean, X.sd) (X.test[, k] - X.mean[k])/X.sd[k],
                     X.test = X.test, X.mean = X.train.mean, X.sd = X.train.sd)
    y.train <- y[-fold[[i]]]
    y.train.mean <- mean(y.train)
    y.train <- y.train - y.train.mean
    y.test <- y[fold[[i]]]
    y.test <- y.test - y.train.mean
    beta.ridge.k <- beta.ridge.estimator(X = X.train, y = y.train, lambda = lambda.seq[j])
    square.errors[i,j] <- sum( (y.test - X.test %*% beta.ridge.k)^2 )
  }
}
# compute the sum of squares
cv.error<-colSums(square.errors)
# find the lambda which minimizes the error
lambda.min<-lambda.seq[which.min(cv.error)]
# compute the estimates
beta.ridge <- c(y.mean, solve(t(X) %*% X + lambda.min * diag(ncol(X))) %*% t(X) %*% y)

round(beta.ridge, 3)


# libraries to perform ridge regression (and lasso and elastic net)
# glmnet (faster, based on numerical approximations)
install.packages('glmnet')
# penalized (more precise, slower)
install.packages('penalized')

# using glmnet
library(glmnet)
lambda.cv <- cv.glmnet(x = X, y = y, alpha = 0)$lambda.min
mod.ridge <- glmnet(y = y, x = X, lambda = lambda.cv, alpha = 0)
round(rbind(y.mean, mod.ridge$beta), 3)

# using book's standardization
lambda.cv.book <- cv.glmnet(x = X.book, y = y, alpha = 0)$lambda.min
mod.ridge.book <- glmnet(y = y, x = X.book, lambda = lambda.cv.book, alpha = 0)
round(rbind(y.mean, mod.ridge.book$beta), 3)


### lasso ###
set.seed(123)
cross.validation.result <- cv.glmnet(x = X, y = y, alpha = 1)
lambda.cv <- cross.validation.result$lambda.min
mod.lasso <- glmnet(y = y, x = X, lambda = lambda.cv, alpha = 1)
round(rbind(y.mean, mod.lasso$beta), 3)

# sparser model
lambda.cv <- cross.validation.result$lambda.1se
mod.lasso <- glmnet(y = y, x = X, lambda = lambda.cv, alpha = 1)
round(rbind(y.mean, mod.lasso$beta), 3)

set.seed(123)
lambda.cv.book <- cv.glmnet(x = X.book, y = y, alpha = 1)$lambda.1se
mod.lasso.book <- glmnet(y = y, x = X.book, lambda = lambda.cv.book, alpha = 1)
round(rbind(y.mean, mod.lasso.book$beta), 3)


### pcr ###
y<-y+y.mean
# compute the rotation coefficients (v)
v <- prcomp(X)$rotation
# compute the principal components
X.pc <- X %*% v
# compute the lieanr model with all component
model.pc <- lm(y ~ ., data = as.data.frame(X.pc))
predict(model.pc)
# note that if we use all components we obtain the LS results
cbind(round(predict(model.1), 3), round(predict(model.pc), 3))
cbind(round(model.1$coefficients, 3),
      round(c(model.pc$coefficients[1], model.pc$coefficients[-1]%*%t(v)), 3))

# using only two components
model.pc.2 <- lm(y ~ PC1 + PC2, data = as.data.frame(X.pc))
y.hat <- model.pc.2$coefficients[1] + model.pc.2$coefficients[2] * X %*% v[,1] +
  model.pc.2$coefficients[3] * X %*% v[,2]
round(model.pc.2$coefficients[2] * v[,1] +  model.pc.2$coefficients[3] * v[,2], 3)

# using the library pls
library(pls)
mod.pcr <- pcr(y ~ ., data=as.data.frame(X), validation = 'CV', segments = 5)
summary(mod.pcr)
mod.pcr$coefficients

# result book
v.book <- prcomp(X.book[, -1])$rotation
mod.pc.book <- lm(y ~ ., data = as.data.frame(X.book[, -1] %*% v.book))
round(v.book %*% mod.pc.book$coefficients[-1], 3)
round(v.book[,1:7] %*% mod.pc.book$coefficients[2:8],3)


### pls ###
mod.pls <- plsr(y ~ ., data=as.data.frame(X), validation = 'CV')
summary(mod.pls)
mod.pls$coefficients

mod.pls.book <- plsr(y ~ ., data=as.data.frame(X.book), validation = 'CV')
summary(mod.pls.book)
mod.pls.book$coefficients
mod.pls.book <- plsr(y ~ .,ncomp = 2, data=as.data.frame(X.book), validation = 'CV')
round(mod.pls.book$coefficients,3)

