i1.bic <- function(X,Y,C,uhat,vhat) {
  bic <- log( norm(Y-X%*%C, type = "F")^2 ) + ( log(ncol(Y)*nrow(Y))/(ncol(Y)*nrow(Y)) * (Matrix::nnzero(uhat) + Matrix::nnzero(vhat) - 1) )
  return(bic)
}
