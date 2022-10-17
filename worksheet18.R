library(Rcpp)
addR <- function(x, y)
{
  return(x + y)
}

cppFunction('int addC(int x, int y) {
int sum = x + y;
return sum;
}')

addR(3,4)
addC(3,4)
#---------------------------------------
#Q1

EucR <- function(x, y)
{
  rtn <- sqrt(sum( (x-y)^2 ))
  return(rtn)
}

cppFunction('double EucC(NumericVector x, NumericVector y) {
double track = 0;
int n = x.size();
for(int i = 0; i < n; i++){
track = track + pow( (x[i] - y[i]), 2);
}
track = sqrt(track);
return track;
}
')

x = runif(1e4)
y = runif(1e4)

benchmark(EucR(x,y), EucC(x,y))

#==========================================

#Q2

func <- function(vec)
{
  n <- length(vec)
  # for tracking sum and log
  sum.log <- 0
  log.of.vec <- numeric(length(n))
  # calculating logs and sum for each element
  for(i in 1:n)
  {
    log.of.vec[i] <- log(vec[i])
    sum.log <- sum.log + log.of.vec[i]
  }
  # fraction
  frac <- log.of.vec/sum.log
  return(frac)
}

func2 = function(vec){
  # calculating the required expression
  return(log(vec) / sum(log(vec)))
}

benchmark(func(1:1e4), func2(1:1e4))


cppFunction(
  'NumericVector funcC(NumericVector vec){
  int n = vec.size();
  double sum_log = 0;
  NumericVector log_of_vec(n);  
  for(int i = 0; i < n; i++){
  log_of_vec[i] = log(vec[i]);
  sum_log = sum_log + log_of_vec[i];
  }
  NumericVector frac(n);
  for(int i = 0; i<n; i++){
  frac[i] = log_of_vec[i] / sum_log;
  };
  return frac;
  }'
)


benchmark(func(1 : 1e4), func2(1 : 1e4), funcC(1 : 1e4))


#------------------------------------------------------------


#Q3

cppFunction(
  'NumericMatrix addM(NumericMatrix A, NumericMatrix B){
  int m = A.nrow();
  int n = A.ncol();
  NumericMatrix M(m,n);
  for(int i = 0; i<m; i++){
  for(int j = 0; j<n; j++){
  M(i,j) = A(i,j) + B(i,j);
  }
  }
  return M;
  }'
)


addM(matrix(1:4, nrow = 2),
     matrix(2 : 5, nrow = 2))
