#include <RcppArmadillo.h>

using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec sfThresh(arma::vec a, arma::vec k)
{
  
int jmax = a.n_rows;
arma::vec alpha(jmax);
    for (int j=0; j < jmax; ++j){
      alpha(j) = std::max(0.0,a(j)-k(j)) - std::max(0.0,-a(j)-k(j)); 
    }
    

  return(alpha); 
}