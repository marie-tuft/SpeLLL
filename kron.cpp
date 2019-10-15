#include <RcppArmadillo.h>
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat kron(arma::mat A,arma::mat B){
  arma::mat C = arma::kron(A,B);
  return(C);
}