install.packages('kdensity')
library(kdensity)

pdf('HG')
concentration = Theoph$conc + 0.001
plot(kdensity(concentration, start = "gamma", kernel = "gamma", adjust = 1/3),
     ylim = c(0, 0.15), lwd = 2, main = "Concentration of theophylline")
lines(kdensity(concentration, start = "gamma", kernel = "gaussian"),
      lty = 2, col = "grey", lwd = 2)
lines(kdensity(concentration, start = "gaussian", kernel = "gaussian"),
      lty = 3, col = "blue", lwd = 2)
lines(kdensity(concentration, start = "gaussian", kernel = "gamma", adjust = 1/3),
      lty = 4, col = "red", lwd = 2)
rug(concentration)
legend('topright', c('start gamma, kernel gamma', 'start gamma, kernel Gaussian',
                     'start Gaussian, kernel Gaussian', 'start Gaussian, kernel gamma'),
       col = c('black','grey','blue','red'), lty = 1:4)
dev.off()

