
#include <RcppArmadillo.h>
#include <soft_thresh.cpp>
#include <setZero.cpp>



using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
arma::mat admmFAST(arma::mat x, arma::vec y, float lam1, 
                   float lam2, float rho,
                   arma::vec initial, arma::mat D,
                   arma::vec m, arma::vec w,
                   float dtol, float ptol, float z_thresh, int maxiter)
  {
  
  arma::vec lambda = join_cols(lam1*w,lam2*m);
  arma::vec alpha = D * initial;
  int t = alpha.n_rows;
  arma::vec z = arma::zeros<arma::vec>(t);
  arma::mat beta_half = inv((x.t()*x) + (rho * D.t()*D));
  arma::vec beta = arma::zeros<arma::vec>(initial.n_rows);

  for (int i = 0; i < maxiter; ++i){
    
    arma::vec alphak=alpha;
    
    beta = beta_half * ((x.t()*y) + (rho*D.t() * (alpha-z)));
    
    arma::vec temp1 = (D * beta)+z;
    arma::vec temp2 = lambda/rho;
    
    alpha = sfThresh(temp1,temp2);

    z = z + (D*beta) - alpha;
    

    arma::vec d = rho * D.t() * (alpha-alphak);
    arma::vec p = (D*beta) - alpha;

    beta = setZero(beta, z_thresh);
    
    if (norm(d,2) < dtol && norm(p,2) < ptol){
          break;
    }
  }

  
  return(beta); 
  }