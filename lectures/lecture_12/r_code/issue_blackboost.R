rm(list = ls())
library(mboost)

# generate toy data
set.seed(123)
X <- matrix(rnorm(20000), ncol = 10)
y <- apply(X^2, 1, sum)
y[y <= qchisq(p = 0.5, df = 10) ] <- -1
y[y > qchisq(p = 0.5, df = 10) ] <- 1

# without specifying non-needed options
boost.10 <- blackboost(as.factor(y) ~ ., data = as.data.frame(X), family = Binomial(),
                       boost_control(mstop = 400), tree_controls = partykit::ctree_control(maxdepth = 10))

# assigning NULL to every option
boost.10 <- blackboost(as.factor(y) ~ ., data = as.data.frame(X), family = Binomial(),
                       weights = NULL, na.action = NULL, offset = NULL,
                       boost_control(mstop = 400), tree_controls = partykit::ctree_control(maxdepth = 10))
