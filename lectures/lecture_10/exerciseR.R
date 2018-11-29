library(ElemStatLearn)
library(rpart)

#load data
data('spam')
dim(spam)

# split in a training a test set
set.seed(12345)
train <- c(sample(which(spam$spam == 'spam'),
                  floor(2*sum(spam$spam == 'spam')/3), replace = FALSE),
           sample(which(spam$spam == 'email'),
                  floor(2*sum(spam$spam == 'email')/3), replace = FALSE))
test <- (1:nrow(spam))[-train]

# single tree
single_tree <- rpart(spam ~ ., data = spam[train, ],
                     control = rpart.control(minsplit = 5))

# bagging
B <- 1e2
bootstrap_tree <- list()

for (i in 1:B)
{
  train_ast <- c(sample(train[1:floor(2*sum(spam$spam == 'spam')/3)],
                              replace = TRUE),
                 sample(train[1:floor(2*sum(spam$spam == 'email')/3)],
                              replace = TRUE))
  bootstrap_tree[[i]] <- rpart(spam ~ ., data = spam[train_ast, ],
                               control = rpart.control(minsplit = 5))
}

# prediction for the single tree
test_error_single_tree <- mean(
  spam[test, ]$spam != predict(single_tree, spam[test, ], type = 'class'))

# prediction bagging
consensus <- NULL
probability <- matrix(0, ncol = 2, nrow = length(test))
for (i in 1:B)
{
  consensus <- cbind(consensus, predict(bootstrap_tree[[i]],
                                        spam[test, ], type = 'class'))
  probability <- probability + predict(bootstrap_tree[[i]], spam[test, ])
}

# prediciton with consensus
consensus <- round(apply(consensus, 1, mean))
consensus[consensus == 1] <- 'email'
consensus[consensus == 2] <- 'spam'
test_error_consensus <- mean(spam[test, ]$spam != consensus)

# prediciton with probability
probability <- probability
probability <- apply(probability, 1, which.max)
probability[probability == 1] <- 'email'
probability[probability == 2] <- 'spam'
test_error_probability <- mean(spam[test, ]$spam != probability)

# comparison
test_error_single_tree
test_error_consensus
test_error_probability
