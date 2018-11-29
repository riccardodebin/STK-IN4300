install.packages("ElemStatLearn")

library(ElemStatLearn)
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
training.error.ls <- sum( (y - G.hat)^2 )
training.percentage.missclassification.ls <- sum( (y - G.hat)^2 )/n # since 1^2=1=(-1)^2

# test set
data(zip.train)
str(zip.test)
response <- zip.test[,1]
X.new <- zip.test[response==2|response==3, -1]
y.new <- response[response==2|response==3]
n.new <- length(y.new)

# prediction
y.hat <- cbind(rep(1, n.new), X.new)%*%beta
G.hat <- ifelse(y.hat <= 2.5, 2, 3) 

# prediciton error with 0-1 loss
test.error.ls <- sum( (y.new - G.hat)^2 )
test.percentage.missclassification.ls <- sum( (y.new - G.hat)^2 )/n.new # since 1^2=1=(-1)^2

### k-nearest neighbours ###
distance <- function(x, x_1) sqrt( sum( (x - x_1)^ 2) )
find.k.neighbours <- function(candidate, other, k)
{
  distances <- apply(other, 1, distance, x_1 = candidate)
  order(distances)[1:k]
}

kNN<-function(X.new, X, y, k)
{
  # X.new = input
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

y.hat_kNN.1 <- kNN(X.new = X, X = X, y = y, k = 1)
G.hat_kNN.1 <- ifelse(y.hat_kNN.1 <= 2.5, 2, 3) 
y.hat_kNN.3 <- kNN(X, X, y, 3)
G.hat_kNN.3 <- ifelse(y.hat_kNN.3 <= 2.5, 2, 3) 
y.hat_kNN.5 <- kNN(X, X, y, 5)
G.hat_kNN.5 <- ifelse(y.hat_kNN.5 <= 2.5, 2, 3) 
y.hat_kNN.7 <- kNN(X, X, y, 7)
G.hat_kNN.7 <- ifelse(y.hat_kNN.7 <= 2.5, 2, 3) 
y.hat_kNN.15 <- kNN(X, X, y, 15)
G.hat_kNN.15 <- ifelse(y.hat_kNN.15 <= 2.5, 2, 3) 

# prediciton error with 0-1 loss
training.error.kNN_1 <- sum( (y - G.hat_kNN.1)^2 )
training.percentage.missclassification.kNN_1 <- sum( (y - G.hat_kNN.1)^2 )/n # since 1^2=1=(-1)^2
training.error.kNN_3 <- sum( (y - G.hat_kNN.3)^2 )
training.percentage.missclassification.kNN_3 <- sum( (y - G.hat_kNN.3)^2 )/n # since 1^2=1=(-1)^2
training.error.kNN_5 <- sum( (y - G.hat_kNN.5)^2 )
training.percentage.missclassification.kNN_5 <- sum( (y - G.hat_kNN.5)^2 )/n # since 1^2=1=(-1)^2
training.error.kNN_7 <- sum( (y - G.hat_kNN.7)^2 )
training.percentage.missclassification.kNN_7 <- sum( (y - G.hat_kNN.7)^2 )/n # since 1^2=1=(-1)^2
training.error.kNN_15 <- sum( (y - G.hat_kNN.15)^2 )
training.percentage.missclassification.kNN_15 <- sum( (y - G.hat_kNN.15)^2 )/n # since 1^2=1=(-1)^2

# test set #
y.hat_kNN.1 <- kNN(X.new = X.new, X = X, y = y, k = 1)
G.hat_kNN.1 <- ifelse(y.hat_kNN.1 <= 2.5, 2, 3) 
y.hat_kNN.3 <- kNN(X.new, X, y, 3)
G.hat_kNN.3 <- ifelse(y.hat_kNN.3 <= 2.5, 2, 3) 
y.hat_kNN.5 <- kNN(X.new, X, y, 5)
G.hat_kNN.5 <- ifelse(y.hat_kNN.5 <= 2.5, 2, 3) 
y.hat_kNN.7 <- kNN(X.new, X, y, 7)
G.hat_kNN.7 <- ifelse(y.hat_kNN.7 <= 2.5, 2, 3) 
y.hat_kNN.15 <- kNN(X.new, X, y, 15)
G.hat_kNN.15 <- ifelse(y.hat_kNN.15 <= 2.5, 2, 3) 

# prediciton error with 0-1 loss
test.error.kNN_1 <- sum( (y.new - G.hat_kNN.1)^2 )
test.percentage.missclassification.kNN_1 <- sum( (y.new - G.hat_kNN.1)^2 )/n # since 1^2=1=(-1)^2
test.error.kNN_3 <- sum( (y.new - G.hat_kNN.3)^2 )
test.percentage.missclassification.kNN_3 <- sum( (y.new - G.hat_kNN.3)^2 )/n # since 1^2=1=(-1)^2
test.error.kNN_5 <- sum( (y.new - G.hat_kNN.5)^2 )
test.percentage.missclassification.kNN_5 <- sum( (y.new - G.hat_kNN.5)^2 )/n # since 1^2=1=(-1)^2
test.error.kNN_7 <- sum( (y.new - G.hat_kNN.7)^2 )
test.percentage.missclassification.kNN_7 <- sum( (y.new - G.hat_kNN.7)^2 )/n # since 1^2=1=(-1)^2
test.error.kNN_15 <- sum( (y.new - G.hat_kNN.15)^2 )
test.percentage.missclassification.kNN_15 <- sum( (y.new - G.hat_kNN.15)^2 )/n # since 1^2=1=(-1)^2

cat('training errors (percentage missallocated)\n')
cat('----------------\n')
cat('LS: ', training.percentage.missclassification.ls, '\n')
cat('KNN(1): ', training.percentage.missclassification.kNN_1, '\n')
cat('KNN(3): ', training.percentage.missclassification.kNN_3, '\n')
cat('KNN(5): ', training.percentage.missclassification.kNN_5, '\n')
cat('KNN(7): ', training.percentage.missclassification.kNN_7, '\n')
cat('KNN(15): ', training.percentage.missclassification.kNN_15, '\n')
cat('######################\n')
cat('test errors (percentage missallocated)\n')
cat('------------\n')
cat('LS: ', test.percentage.missclassification.ls, '\n')
cat('KNN(1): ', test.percentage.missclassification.kNN_1, '\n')
cat('KNN(3): ', test.percentage.missclassification.kNN_3, '\n')
cat('KNN(5): ', test.percentage.missclassification.kNN_5, '\n')
cat('KNN(7): ', test.percentage.missclassification.kNN_7, '\n')
cat('KNN(15): ', test.percentage.missclassification.kNN_15, '\n')

