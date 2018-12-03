# load package with the data
library(ElemStatLearn)
# load package with Hjort-Glad density approach
library(kdensity)

### variable tobacco ###
# plots using the kernel density
plot(density(SAheart$tobacco))
# same using the Epanechnikov kernel instead of a Gaussian one
plot(density(SAheart$tobacco, kernel ='epanechnikov'))
# use the Hjort-Glad method with a Gaussian start
plot(kdensity(SAheart$alcohol, start = "normal"))
# use the Hjort-Glad method with an esponential start
plot(kdensity(SAheart$tobacco, start = "exponential"))
# note that the support is correct!!!

### variable alcohol ###
plot(density(SAheart$alcohol))
plot(density(SAheart$alcohol, kernel ='epanechnikov'))
plot(kdensity(SAheart$alcohol, start = "normal"))
plot(kdensity(SAheart$alcohol, start = "exponential"))

