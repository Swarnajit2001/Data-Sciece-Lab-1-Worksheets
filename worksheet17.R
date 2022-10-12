library(rbenchmark)
library(profvis)

#Q1

num1 <- numeric(length = 1e3)
num2 <- numeric(length = 1e6)
mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
mat2 <- matrix(0, nrow = 100, ncol = 1000)
arr <- array(0, dim = c(100,100,100))

object.size(num1)
object.size(num2)
object.size(mat1)
object.size(mat2)
object.size(arr)


#Q2

benchmark(
  {
    num = numeric(1e4)
    for(i in 1 : 1e4)
      num[i] = rnorm(1)
  },
  rnorm(1e4),
  replications = 100)

#Q3

benchmark(
  rnorm(n = 1e4),
  runif(n = 1e4),
  replications = 100
)

#Q4

rho = 5
n = c(10,100,1000)

#with loop
rho_mat_1 = function(n, rho){
  m = matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n)
      m[i,j] = rho^(abs(i - j))
  }
  return(m)
}



#without loop
rho_mat_2 <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^(abs(col(mat) - row(mat)))
  return(mat)
}


benchmark(rho_mat_1(n[1],rho),
          rho_mat_2(n[1],rho),
          replications = 100)

benchmark(rho_mat_1(n[2],rho),
          rho_mat_2(n[2],rho),
          replications = 100)

benchmark(rho_mat_1(n[3],rho),
          rho_mat_2(n[3],rho),
          replications = 100)



#Q5

stirling = function(n){
  s = factorial(n) / ((n^n/exp(n)) * sqrt(2*pi*n))
  return(s)
}

par(mfrow = c(2,3))

n = 10^(1 : 6)

for(i in 1 : 6){
  plot(stirling(1 : n[i]), pch = 19)
}

par(mfrow = c(1,1))

stirling_modified = function(n){
  s = exp(sum(log(1 : n)) - n * log(n) + n - log(2 * pi * n)/ 2)
  return(s)
}

par(mfrow = c(2,3))

n = 10^(1 : 6)

for(i in 1 : 6){
  plot(stirling_modified(1 : n[i]), pch = 19)
}

par(mfrow = c(1,1))