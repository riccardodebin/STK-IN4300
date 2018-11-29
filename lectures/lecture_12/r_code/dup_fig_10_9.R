# 
# Written by:
# -- 
# John L. Weatherwax                2007-07-05
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

###install.packages('gbm')
library(gbm)

set.seed(0)

gen_eq_10_2_data = function(N=100,p=10){
  X = matrix( rnorm( N*p ), nrow=N, ncol=p )
  Y = matrix( -1, nrow=N, ncol=1 )
  threshold = qchisq(0.5,df=p)
  indx = rowSums( X^2 ) > threshold
  Y[indx] = +1
  data.frame(X,Y)
}

testing_error_vs_n_boosts = function(m,D_test){
  # 
  # Estimate the test set learning curve 
  # 
  
  n_trees = m$n.trees
  
  test_error = matrix( 0, nrow=n_trees, ncol=1 )
  for( nti in seq(1,n_trees) ){
    Fhat = predict( m, D_test[,1:p], n.trees=nti )
    pcc = mean( ( ( Fhat <= 0 ) & ( D_test[,p+1] == 0 ) ) | ( ( Fhat > 0 ) & ( D_test[,p+1] == 1 ) ) )
    test_error[nti] = 1 - pcc 
  }

  test_error 
}

p = 10 # the dimension of the feature vector (a "p" dimensional vector)
N_train = 2000 # number samples to use in training
N_test = 10000 # number of samples to use in testing 

# Extract the data we will to classify: 
D_train = gen_eq_10_2_data(N=N_train,p=p)
D_train[ D_train$Y==-1, p+1 ] = 0 # Map the response "-1" to the value of "0" (required format for the call to gbm): 

# Generate the formula used to fit our model with: 
#
terms = paste( colnames(D_train)[1:p], collapse="+" ) # dont consider the last column (the response variable)
formula = formula( paste( colnames(D_train)[p+1], " ~ ", terms ) )

# Extract the data we will test with: 
D_test = gen_eq_10_2_data(N=N_test,p=p)
D_test[ D_test$Y==-1, p+1 ] = 0 # Map the response "-1" to the value of "0" (required format for the call to gbm): 

# The maximum number of trees to train with:
# 
n_trees = 400

print( "Training Adaboost ..." )
m = gbm( formula, data=D_train, distribution='adaboost', n.trees=n_trees, shrinkage=1, verbose=TRUE )
ab_testing_error = testing_error_vs_n_boosts(m,D_test)

print( "Running Binomial Deviance Boosting J=1 (stumps) ..." )
m = gbm( formula, data=D_train, distribution='bernoulli', interaction.depth=1, shrinkage=1, n.trees=n_trees, verbose=TRUE )
bd1_testing_error = testing_error_vs_n_boosts(m,D_test)

print( "Running Binomial Deviance Boosting J=6 ..." )
m = gbm( formula, data=D_train, distribution='bernoulli', interaction.depth=6, shrinkage=1, n.trees=n_trees, verbose=TRUE )
bd6_testing_error = testing_error_vs_n_boosts(m,D_test)

print( "Running Binomial Deviance Boosting J=20 ..." )
m = gbm( formula, data=D_train, distribution='bernoulli', interaction.depth=20, shrinkage=1, n.trees=n_trees, verbose=TRUE )
bd20_testing_error = testing_error_vs_n_boosts(m,D_test)

# Lets plot the testing error as a function of the number of trees for each of the potential models:
#
#postscript("../../WriteUp/Graphics/Chapter10/dup_fig_10_9.eps", onefile=FALSE, horizontal=FALSE)

plot( seq(1,n_trees), ab_testing_error, ylim=c(0,0.4), type="l", main="Boosting Probability of Error", col="gray", xlab="number of boosting iterations", ylab="classification error" )
lines( seq(1,n_trees), bd1_testing_error, type="l", col="black" )
lines( seq(1,n_trees), bd6_testing_error, type="l", col="green" )
lines( seq(1,n_trees), bd20_testing_error, type="l", col="red" )

legend( 300, 0.38, c("AdaBoost", "Stumps", "10 Node", "20 Node"), col=c("gray", "black", "green", "red"), lty=c(1,1) )

#dev.off()



