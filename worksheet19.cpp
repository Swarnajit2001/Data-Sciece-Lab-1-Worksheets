#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//Q1
//---

// [[Rcpp::export]]
double EucC(NumericVector x, NumericVector y) {
  NumericVector z = x - y;
  return sqrt(sum(pow(z,2)));
}


//Q2
//---
// [[Rcpp::export]]

NumericVector funcC(NumericVector vec){
  NumericVector z = log(vec);
  return z / sum(z);
}


//Q3
//---
// [[Rcpp::export]]

NumericMatrix addM(NumericMatrix A, NumericMatrix B) {
  int m = A.nrow();
  int n = A.ncol();
  NumericMatrix Z(m,n);
  for(int i = 0; i<m; i++){
    for(int j = 0; j<n; j++){
      Z(i,j) = A(i,j) + B(i,j);
    }
  }
  return Z;
}

//Q4
//---
// [[Rcpp::export]]

NumericVector sumx(NumericVector x, NumericVector y){
  
}

