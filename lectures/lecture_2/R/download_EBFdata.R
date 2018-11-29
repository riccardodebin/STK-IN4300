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
