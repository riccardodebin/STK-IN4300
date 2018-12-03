library(class) # package for kNN
library(ElemStatLearn) # package with the data
data(zip.train)
str(zip.train)
image(zip2image(zip.train, 2), col = gray((32:0)/32))

# 7291 observations in the training set
head(zip.train)
# first column is the response, then 16x16=256 pixel intensities
response <- zip.train[ ,1]
X <- zip.train[response==2|response==3, -1]
y <- response[response==2|response==3]
n <- length(y)

### least square ###
model <- lm(y ~ X)
beta <- model$coefficients
y.hat <- cbind(rep(1, n), X)%*%beta
G.hat <- ifelse(y.hat <= 2.5, 2, 3) 

# prediciton error with 0-1 loss
training.error.ls <- sum( (y - G.hat)^2 ) / n

# test set
data(zip.test)
str(zip.test)
response <- zip.test[, 1]
X.new <- zip.test[response==2|response==3, -1]
y.new <- response[response==2|response==3]
n.new <- length(y.new)

# prediction
y.hat <- cbind(rep(1, n.new), X.new)%*%beta
G.hat <- ifelse(y.hat <= 2.5, 2, 3) 

# prediciton error with 0-1 loss
test.error.ls <- sum( (y.new - G.hat)^2 ) / length(y.new)

### k-nearest neighbours ###
y.hat_kNN.1 <- knn(train = X, test = X, cl = y, k = 1)
y.hat_kNN.3 <- knn(train = X, test = X, cl = y, k = 3)
y.hat_kNN.5 <- knn(train = X, test = X, cl = y, k = 5)
y.hat_kNN.7 <- knn(train = X, test = X, cl = y, k = 7)
y.hat_kNN.15 <- knn(train = X, test = X, cl = y, k = 15)

# prediciton error with 0-1 loss
tmp <- table(y, y.hat_kNN.1)
training.error.kNN_1 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y, y.hat_kNN.3)
training.error.kNN_3 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y, y.hat_kNN.5)
training.error.kNN_5 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y, y.hat_kNN.7)
training.error.kNN_7 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y, y.hat_kNN.15)
training.error.kNN_15 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)

# test set #
y.hat_kNN.1 <- knn(train = X, test = X.new, cl = y, k = 1)
y.hat_kNN.3 <- knn(train = X, test = X.new, cl = y, k = 3)
y.hat_kNN.5 <- knn(train = X, test = X.new, cl = y, k = 5)
y.hat_kNN.7 <- knn(train = X, test = X.new, cl = y, k = 7)
y.hat_kNN.15 <- knn(train = X, test = X.new, cl = y, k = 15)

# prediciton error with 0-1 loss
tmp <- table(y.new, y.hat_kNN.1)
test.error.kNN_1 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y.new, y.hat_kNN.3)
test.error.kNN_3 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y.new, y.hat_kNN.5)
test.error.kNN_5 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y.new, y.hat_kNN.7)
test.error.kNN_7 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)
tmp <- table(y.new, y.hat_kNN.15)
test.error.kNN_15 <- (tmp[1, 2] + tmp[2, 1])/sum(tmp)

cat('\n')
cat('training errors (percentage missallocated)\n')
cat('----------------\n')
cat('LS: ', training.error.ls, '\n')
cat('KNN(1): ', training.error.kNN_1, '\n')
cat('KNN(3): ', training.error.kNN_3, '\n')
cat('KNN(5): ', training.error.kNN_5, '\n')
cat('KNN(7): ', training.error.kNN_7, '\n')
cat('KNN(15): ', training.error.kNN_15, '\n')
cat('\n')
cat('test errors (percentage missallocated)\n')
cat('------------\n')
cat('LS: ', test.error.ls, '\n')
cat('KNN(1): ', test.error.kNN_1, '\n')
cat('KNN(3): ', test.error.kNN_3, '\n')
cat('KNN(5): ', test.error.kNN_5, '\n')
cat('KNN(7): ', test.error.kNN_7, '\n')
cat('KNN(15): ', test.error.kNN_15, '\n')


# writing manually function kNN
kNN<-function(X.new, X, y, k)
{
  # X.new = test data
  # X = training data
  # y = response training data
  # k = number of neighbours
  
  # first iteration
  neighbours <- find.k.neighbours(X.new[1,], X, k)
  y.neighbours <- y[neighbours]
  y.hat <- mean(y.neighbours)
  
  for (i in 2:nrow(X.new))
  {
    neighbours <- find.k.neighbours(X.new[i, ], X, k)
    y.hat<-c( y.hat, mean( y[neighbours] ) )
    print(i)
  }
  y.hat
}
