# download data
download.file('https://www.imbi.uni-freiburg.de/imbi/Royston-Sauerbrei-book/Multivariable_Model-building/downloads/datasets/edu_bodyfat_both.zip',
              'dataEdu_BodyFat.zip')
data <- read.csv(unz('dataEdu_BodyFat.zip',
                     'edu_bodyfat_both/edu_bodyfat/edu_bodyfat.csv'),
                 header = TRUE)
system('rm dataEdu_BodyFat.zip')
# detect the response variable
y <- data[, 15]
# select the explanatory variables
X <- data[, 2:14]
rm(data)
boxplot(scale(X))
points(1:13, scale(X)[39, ], pch = 16, col = 2)
y <- y[-39]
X <- X[-39, ]
boxplot(scale(X))

full.model <- lm(y~., data = as.data.frame(X))
null.model <- lm(y~1, data = as.data.frame(X))

### backward elimination ###
summary(full.model)
model_2<-lm(y ~ age + weight + height + neck + chest + ab + hip + thigh + ankle + biceps +
              forearm + wrist, data=as.data.frame(X))
summary(model_2)
model_3<-lm(y ~ age + height + neck + chest + ab + hip + thigh + ankle + biceps +
              forearm + wrist, data=as.data.frame(X))
summary(model_3)
model_4<-lm(y ~ age + height + neck + chest + ab + hip + thigh + biceps +
              forearm + wrist, data=as.data.frame(X))
summary(model_4)
model_5<-lm(y ~ age + height + neck + chest + ab + hip + thigh +
              forearm + wrist, data=as.data.frame(X))
summary(model_5)
model_6<-lm(y ~ age + height + neck + ab + hip + thigh +
              forearm + wrist, data=as.data.frame(X))
summary(model_6)
model_7<-lm(y ~ age + height + neck + ab + thigh +
              forearm + wrist, data=as.data.frame(X))
summary(model_7)
model_8<-lm(y ~ age + height + neck + ab +
              forearm + wrist, data=as.data.frame(X))
summary(model_8)
model_9<-lm(y ~ age + height + neck + ab +
               wrist, data=as.data.frame(X))
summary(model_9)
model_10<-lm(y ~ age + height + ab +
              wrist, data=as.data.frame(X))
summary(model_10)
model.backward <- model_10

add1(object = null.model, scope = full.model)  
model_1 <- lm(y ~ ab, data = as.data.frame(X))
summary(model_1)
add1(object = model_1, scope = full.model)  
model_2 <- lm(y ~ ab + weight, data = as.data.frame(X))
summary(model_2)
add1(object = model_2, scope = full.model)  
model_3 <- lm(y ~ ab + weight + wrist, data = as.data.frame(X))
summary(model_3)
add1(object = model_3, scope = full.model)  
model_4 <- lm(y ~ ab + weight + wrist + biceps, data = as.data.frame(X))
summary(model_4)
model.forward <- model_3

# automatically...
model.backward.aic <- stepAIC(object = full.model, scope = null.model, direction = 'backward')
model.stepback.aic <- stepAIC(object = full.model, scope = null.model, direction = 'both')
model.forward.aic <- stepAIC(object = null.model, scope = full.model$terms, direction = 'forward')
model.stepwise.aic <- stepAIC(object = null.model, scope = full.model$terms, direction = 'both')


### all subset ###
allCombinations <- sapply(1:13, function(m) combn(x = 1:13, m = m))

null.model <- lm(y ~ 1)
result.AIC <- extractAIC(null.model)
result.RSS <- cbind(1, deviance(null.model))
for (i in 1:13) # number of combinations
{
  for (j in 1:ncol(allCombinations[[i]]))
  {
    model<-lm(y ~ ., data = as.data.frame(X[, allCombinations[[i]][, j]]))
    result.AIC <- rbind(result.AIC, extractAIC(model))
    result.RSS <- rbind(result.RSS, cbind(length(allCombinations[[i]][, j]), deviance(model)))
  }
}

plot(result.RSS[, 1], result.RSS[, 2], main = 'Plot', xlab = 'Subset size',
     ylab = 'Residual sum of squares')

# identify the model with the smallest AIC
best.AIC <- result.AIC[which.min(result.AIC[, 2]), ]
index <- which.min(result.AIC[result.AIC[, 1] == best.AIC[1], 2])
variables.best.model <- allCombinations[[best.AIC[1] - 1]][, index]

# final model
model.bestSubset.AIC <- lm(y ~ ., data = as.data.frame(X[, variables.best.model]))
summary(model.bestSubset.AIC)




