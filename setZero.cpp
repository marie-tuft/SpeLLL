#include <RcppArmadillo.h>

using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec setZero(arma::vec a, double thresh){
  int length = a.n_rows;
  arma::vec aa = abs(a);
  for (int i = 0; i < length; ++i){
    if (aa(i) < thresh){
      a(i) = 0.0;
    }
  }
  
  return(a);
}