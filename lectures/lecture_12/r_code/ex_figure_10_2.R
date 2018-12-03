rm(list = ls())

library(mboost)
library(rpart)

# generate train data
set.seed(123)
X <- matrix(rnorm(20000), ncol = 10)
y <- apply(X^2, 1, sum)
y[y <= qchisq(p = 0.5, df = 10) ] <- -1
y[y > qchisq(p = 0.5, df = 10) ] <- 1
table(y)
N <- length(y)

# generate test data
set.seed(123)
X.test <- matrix(rnorm(100000), ncol = 10)
y.test <- apply(X.test^2, 1, sum)
y.test[y.test <= qchisq(p = 0.5, df = 10) ] <- -1
y.test[y.test > qchisq(p = 0.5, df = 10) ] <- 1
table(y.test)
N.test <- length(y.test)

# single stump
single.stump <- rpart(as.factor(y)~., data = as.data.frame(X),
                      control = rpart.control(maxdepth = 1))

# 244 node tree
single.tree <- rpart(as.factor(y)~., data = as.data.frame(X),
                     control = rpart.control(maxdepth = 30))

# boosting
boosting <- mboost(as.factor(y) ~ ., data = as.data.frame(X),
                   family = Binomial(), baselearner = 'btree',
                   boost_control(mstop = 400))

### test errors ###
# single stump
y.stump <- predict(single.stump, newdata = as.data.frame(X.test),
                   type = 'class')
error.stump <- mean(as.factor(y.test) != y.stump)
# 244 node tree
y.tree <- predict(single.tree, newdata = as.data.frame(X.test),
                  type = 'class')
error.tree <- mean(as.factor(y.test) != y.tree)
# boosting
y.boosting <- matrix(NA, ncol = 400, nrow = 10000)
error.boosting <- NULL
for (i in 1:400)
{
  y.boosting[ , i] <- as.factor(predict(boosting[i],
                                        newdata = as.data.frame(X.test),
                                        type = 'class'))
  error.boosting[i] <- mean(y.test != 2*(y.boosting[ , i] - 1.5))
}


# plot
plot(1:400, error.boosting, type = 'l', col = 'orange', ylim = c(0, 0.5),
     xlab = 'Boosting iterations', ylab = 'Test error')
abline(h = error.stump, lty = 2)
text(200, 0.48, 'Single Stump')
abline(h = error.tree, lty = 2)
text(200, 0.29, 'Single Tree')

