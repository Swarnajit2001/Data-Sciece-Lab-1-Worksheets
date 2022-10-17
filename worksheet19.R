library(Rcpp)
library(RcppArmadillo)

sourceCpp('worksheet19.cpp')

EucC(c(1,1,1),c(0,0,0))


funcC(1 : 3)


addM(matrix(1:4, nrow = 2),
     matrix(2 : 5, nrow = 2))

