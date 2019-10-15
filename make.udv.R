make.udv <- function(X,Y,gamma){
  rrols <- rrpack::rrr(Y, X, penaltySVD="rank", ic.type="BIC",df.type="naive", maxrank=1)
  
  C_tilde_svd <- svd(rrols$coef,nu=1,nv=1) 
  
  u <- C_tilde_svd$u/norm(C_tilde_svd$u, type = "f")
  v <- C_tilde_svd$v/norm(C_tilde_svd$v, type = "f")
  d <- C_tilde_svd$d[1]
  
  #Weights
  w_d <- abs(d)^(-gamma)
  w_u <- abs(u)^(-gamma)
  w_v <- abs(v)^(-gamma)
  m_u <- abs(diff(u))^(-gamma)
  #m_v <- abs(diff(v))^(-gamma)
  
  w_u[which(w_u == Inf)] <- 999999
  w_v[which(w_v == Inf)] <- 999999
  
  return(list(u0=u, v0=v, d0=d, w_d=w_d, w_u=w_u, w_v=w_v, m_u=m_u))
}
