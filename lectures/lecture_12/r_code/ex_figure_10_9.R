###########################################################################################################
# NOTE: boosting with 10-node trees and 100-node trees as base learners is not working. I will fix that,  #
#       in the meantime see https://www.waxworksmath.com/Authors/G_M/Hastie/Code/Chapter10/dup_fig_10_9.R #
###########################################################################################################

rm(list = ls())

library(mboost)
library(rpart)

# generate train data
set.seed(123)
X <- matrix(rnorm(20000), ncol = 10)
y <- apply(X^2, 1, sum)
y[y <= qchisq(p = 0.5, df = 10) ] <- -1
y[y > qchisq(p = 0.5, df = 10) ] <- 1

# generate test data
set.seed(123)
X.test <- matrix(rnorm(100000), ncol = 10)
y.test <- apply(X.test^2, 1, sum)
y.test[y.test <= qchisq(p = 0.5, df = 10) ] <- -1
y.test[y.test > qchisq(p = 0.5, df = 10) ] <- 1

# AdaBoost
AdaBoost <- mboost(as.factor(y)~., data = as.data.frame(X),
                   family = AdaExp(), baselearner = 'btree',
                   boost_control(mstop = 400))
# boosting with stumps
boost.1 <- mboost(as.factor(y) ~ ., data = as.data.frame(X), family = Binomial(), baselearner = 'btree',
                  boost_control(mstop = 400))
# boosting with 10-node trees
boost.10 <- blackboost(as.factor(y) ~ ., data = as.data.frame(X), weights = NULL,
                   family = Binomial(), na.action = NULL, offset = NULL,
                   boost_control(mstop = 400), tree_controls = partykit::ctree_control(maxdepth = 10))
# boosting with 100-node trees
boost.100 <- blackboost(as.factor(y) ~ ., data = as.data.frame(X), weights = NULL,
                       family = Binomial(), na.action = NULL, offset = NULL,
                       boost_control(mstop = 400), tree_controls = partykit::ctree_control(maxdepth = 100))

error.AdaBoost <- NULL
error.boost.1 <- NULL
error.boost.10 <- NULL
error.boost.100 <- NULL
for (i in 1:400)
{
  tmp <- as.factor(predict(AdaBoost[i], newdata = as.data.frame(X.test), type = 'class'))
  error.AdaBoost[i] <- mean(y.test != tmp)
  tmp <- as.factor(predict(boost.1[i], newdata = as.data.frame(X.test), type = 'class'))
  error.boost.1[i] <- mean(y.test != tmp)
  tmp <- as.factor(predict(boost.10[i], newdata = as.data.frame(X.test), type = 'class'))
  error.boost.10[i] <- mean(y.test != tmp)
  tmp <- as.factor(predict(boost.100[i], newdata = as.data.frame(X.test), type = 'class'))
  error.boost.100[i] <- mean(y.test != tmp)
}


# plot
plot(1:400, error.AdaBoost, type = 'l', col = 'gray', ylim = c(0, 0.5),
     xlab = 'Boosting iterations', ylab = 'Test error')
lines(1:400, error.boost.1, type = 'l', col = 'green')
lines(1:400, error.boost.10, type = 'l', col = 'blue')
lines(1:400, error.boost.100, type = 'l', col = 'orange')
legend('topright', c('AdaBoost', 'stumps', '10 nodes', '100 nodes'), lty = rep(1,4), 
       col = c('gray', 'green', 'blue', 'orange'))

