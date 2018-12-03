library(ElemStatLearn)
library(rpart)

# load the data
data('spam')

set.seed(12345)
# split in training and test data
train <- c(sample(which(spam$spam == 'spam'), floor(2*sum(spam$spam == 'spam')/3), replace = FALSE),
           sample(which(spam$spam == 'email'), floor(2*sum(spam$spam == 'email')/3), replace = FALSE))
test <- (1:nrow(spam))[-train]

# single tree
single_tree <- rpart(spam ~ ., data = spam[train, ], control = rpart.control(minsplit = 5))

# bagging
B <- 1e2
bootstrap_trees <- list()

for (i in 1:B)
{
  train_ast <- c(sample(train[1:floor(2*sum(spam$spam == 'spam')/3)], replace = TRUE),
                 sample(train[-c(1:floor(2*sum(spam$spam == 'spam')/3))], replace = TRUE))
  bootstrap_trees[[i]] <- rpart(spam ~ ., data = spam[train_ast, ], control = rpart.control(minsplit = 5))
}

# prediction error single tree
test_error_single_tree <- mean(spam[test, ]$spam != predict(single_tree, spam[test, ], type = 'class'))

# prediction using bagging
consensus <- NULL
probability <- matrix(0, ncol = 2, nrow = length(test))
for (i in 1:B)
{
  consensus <- cbind(consensus, predict(bootstrap_trees[[i]], spam[test, ], type = 'class'))
  probability <- probability + predict(bootstrap_trees[[i]], spam[test, ])
}

# prediction error consensus
consensus <- round(apply(consensus,1,mean))
consensus[consensus == 1] <- 'email'
consensus[consensus == 2] <- 'spam'
test_error_consensus <- mean(spam[test, ]$spam != consensus)

# prediction error probability
probability <- apply(probability, 1, which.max)
probability[probability == 1] <- 'email'
probability[probability == 2] <- 'spam'
test_error_probability <- mean(spam[test, ]$spam != probability)


test_error_single_tree
test_error_consensus
test_error_probability
