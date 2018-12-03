### solution ex. 5.3 ###
# load package for splines
library(splines)

# fix a seed (for reproducibility)
set.seed(111)
# fix number of points
n_points <- 50
# generate input and response
x <- runif(n_points, 0, 1)
x <- sort(x) # just for easiness to display
y <- x + rnorm(n_points)

# Global Linear
X <- cbind(rep(1, n_points), x)
H_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
variance_linear <- diag(H_matrix)

# Global Cubic Polynomial (polynomial of order 3)
X <- cbind(X, x^2, x^3)
H_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
variance_polynomial <- diag(H_matrix)

# splines
# cubic spline 2 knots
B <- bs(x, degree = 3, knots = c(0.33, 0.66), intercept = TRUE)
H_matrix <- B %*% solve(t(B) %*% B) %*% t(B)
variance_cubicSpline2 <- diag(H_matrix)

# natural spline 6 knots
knots <- c(0.1, 0.26, 0.42, 0.58, 0.74, 0.9)
N <- matrix(NA, n_points, 6)
N[ , 1] <- rep(1, n_points)
N[ , 2] <- x
d_Kminus1 <- (pmax(x - knots[5], 0)^3 - pmax(x - knots[6], 0)^3) / (knots[6] - knots[5])
for (k in 1:4)
{
 N[ , k + 2] <- (pmax(x - knots[k], 0)^3 - pmax(x - knots[6], 0)^3) / (knots[6] - knots[k])
 N[ , k + 2] <- N[ , k + 2] - d_Kminus1
}
H_matrix <- N %*% solve(t(N) %*% N) %*% t(N)
variance_naturalSpline6 <- diag(H_matrix)

# plot Figure 5.3
plot(c(0, 1), c(0, 0.65), type = "n", xlab = "x", ylab = "Pointwise variance")
points(x, variance_linear, pch = 16, col = 'orange')
points(x, variance_linear, type = "l", col = 'orange')
points(x, variance_polynomial, pch = 16, col = 'red')
points(x, variance_polynomial, type = "l", col = 'red')
points(x, variance_cubicSpline2, pch = 16, col = 'green')
points(x, variance_cubicSpline2, type = "l", col = 'green')
points(x, variance_naturalSpline6, pch = 16, col = 'blue')
points(x, variance_naturalSpline6, type = "l", col = 'blue')
legend(0.3, 0.65, c("Global Linear",
                  "Global Cubic Polynamial",
                  "Cubic Spline - 2 knots",
                  "Natural Cubic Splines - 6 knots"),
       lty = 1, col = c('orange', 'red', 'green', 'blue'))

