# exercise 16.4 #

library(glmnet)

# initialize values
N <- 50
p <- 300
p_mid <- 10
p_bot <- 30

single_run <- function(i, N, p, p_mid, p_bot, signal_to_noise)
{
  result <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  names(result) <- c('lasso_cont_top', 'lasso_cont_mid', 'lasso_cont_bot',
                     'ridge_cont_top', 'ridge_cont_mid', 'ridge_cont_bot',
                     'lasso_bin_top', 'lasso_bin_mid', 'lasso_bin_bot',
                     'ridge_bin_top', 'ridge_bin_mid', 'ridge_bin_bot')
  set.seed(i)
  # generate X
  X <- matrix(rnorm(N * p), ncol = p, nrow = N)
  
  # generate the regression coefficients
  beta_top <- rnorm(p)
  beta_mid <- c(rnorm(p_mid), rep(0, p - p_mid))
  beta_bot <- c(rnorm(p_bot), rep(0, p - p_bot))

  # generate the responses
  # continuous
  Y_top <- X%*%beta_top + rnorm(N, sd = sqrt(var(X%*%beta_top)*signal_to_noise))
  Y_mid <- X%*%beta_mid + rnorm(N, sd = sqrt(var(X%*%beta_mid)*signal_to_noise))
  Y_bot <- X%*%beta_bot + rnorm(N, sd = sqrt(var(X%*%beta_bot)*signal_to_noise))
  # binomial
  epsilon <- rnorm(N, sd = sqrt(var(X%*%beta_top)*signal_to_noise))
  G_top <- rbinom(n = N, size = 1, prob = exp(X%*%beta_top + epsilon)/(1 + exp(X%*%beta_top + epsilon)))
  epsilon <- rnorm(N, sd = sqrt(var(X%*%beta_mid)*signal_to_noise))
  G_mid <- rbinom(n = N, size = 1, prob = exp(X%*%beta_mid + epsilon)/(1 + exp(X%*%beta_mid + epsilon)))
  epsilon <- rnorm(N, sd = sqrt(var(X%*%beta_bot)*signal_to_noise))
  G_bot <- rbinom(n = N, size = 1, prob = exp(X%*%beta_bot + epsilon)/(1 + exp(X%*%beta_bot + epsilon)))
  
  # find lambda
  lambda <- cv.glmnet(x = X, y = Y_top, alpha = 1)
  result[1] <- glmnet(x = X, y = Y_top, alpha = 1, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = Y_mid, alpha = 1)
  result[2] <- glmnet(x = X, y = Y_mid, alpha = 1, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = Y_bot, alpha = 1)
  result[3] <- glmnet(x = X, y = Y_bot, alpha = 1, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = Y_top, alpha = 0)
  result[4] <- glmnet(x = X, y = Y_top, alpha = 0, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = Y_mid, alpha = 0)
  result[5] <- glmnet(x = X, y = Y_mid, alpha = 0, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = Y_bot, alpha = 0)
  result[6] <- glmnet(x = X, y = Y_bot, alpha = 0, lambda = lambda$lambda.min)$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_top, alpha = 1, family = 'binomial')
  result[7] <- glmnet(x = X, y = G_top, alpha = 1, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_mid, alpha = 1, family = 'binomial')
  result[8] <- glmnet(x = X, y = G_mid, alpha = 1, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_bot, alpha = 1, family = 'binomial')
  result[9] <- glmnet(x = X, y = G_bot, alpha = 1, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_top, alpha = 0, family = 'binomial')
  result[10] <- glmnet(x = X, y = G_top, alpha = 0, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_mid, alpha = 0, family = 'binomial')
  result[11] <- glmnet(x = X, y = G_mid, alpha = 0, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  lambda <- cv.glmnet(x = X, y = G_bot, alpha = 0, family = 'binomial')
  result[12] <- glmnet(x = X, y = G_bot, alpha = 0, lambda = lambda$lambda.min, family = 'binomial')$dev.ratio
  
  result
}

all1 <- sapply(1:4, single_run, N = N, p = p, p_mid = p_mid, p_bot = p_bot, signal_to_noise = 0.1)
all2 <- sapply(1:4, single_run, N = N, p = p, p_mid = p_mid, p_bot = p_bot, signal_to_noise = 0.2)
all3 <- sapply(1:4, single_run, N = N, p = p, p_mid = p_mid, p_bot = p_bot, signal_to_noise = 0.3)
all4 <- sapply(1:4, single_run, N = N, p = p, p_mid = p_mid, p_bot = p_bot, signal_to_noise = 0.4)
all5 <- sapply(1:4, single_run, N = N, p = p, p_mid = p_mid, p_bot = p_bot, signal_to_noise = 0.5)

plot_tl <- cbind(all1[1, ], all2[1, ], all3[1, ], all4[1, ], all5[1, ],
                 all1[4, ] ,all2[4, ], all3[4, ], all4[4, ], all5[4, ])
plot_tr <- cbind(all1[7, ], all2[7, ], all3[7, ], all4[7, ], all5[7, ],
                 all1[10, ] ,all2[10, ], all3[10, ], all4[10, ], all5[10, ])
plot_ml <- cbind(all1[2, ], all2[2, ], all3[2, ], all4[2, ], all5[2, ],
                 all1[5, ] ,all2[5, ], all3[5, ], all4[5, ], all5[5, ])
plot_mr <- cbind(all1[8, ], all2[8, ], all3[8, ], all4[8, ], all5[8, ],
                 all1[11, ] ,all2[11, ], all3[11, ], all4[11, ], all5[11, ])
plot_bl <- cbind(all1[3, ], all2[3, ], all3[3, ], all4[3, ], all5[3, ],
                 all1[6, ] ,all2[6, ], all3[6, ], all4[6, ], all5[6, ])
plot_br <- cbind(all1[1, ], all2[1, ], all3[1, ], all4[1, ], all5[1, ],
                 all1[10, ] ,all2[10, ], all3[10, ], all4[10, ], all5[10, ])

par(mfrow = c(3,2))
boxplot(plot_tl, ylim = c(0, 1), names = c('0.1', '0.2', '0.3', '0.4',
                                           '0.5', '0.1', '0.2', '0.3',
                                           '0.4', '0.5'))
boxplot(plot_tr, ylim = c(0, 1))
boxplot(plot_ml, ylim = c(0, 1))
boxplot(plot_mr, ylim = c(0, 1))
boxplot(plot_bl, ylim = c(0, 1))
boxplot(plot_br, ylim = c(0, 1))


