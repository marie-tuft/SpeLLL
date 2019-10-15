decomp.layer <- function(X, Y, rank, C = NULL, gamma = .25){
  if (is.null(C)){
  C <- rrpack::rrr(Y,X, penaltySVD="rank", ic.type="BIC",df.type="naive", maxrank=min(ncol(Y),ncol(X)))$coef
  }
  
#  C_tilde_svd <- svd(C,nu=rank,nv=rank)
  C_tilde_svd <- svd(C)
  
  layers <- vector("list",rank)
  
  for (i in 1:rank){
    u <-  C_tilde_svd$u[,i]
    d <-  C_tilde_svd$d[i]
    v <-  C_tilde_svd$v[,i]
    
    w_d <- abs(d)^(-gamma)
    w_u <- abs(u)^(-gamma)
    w_v <- abs(v)^(-gamma)
    m_u <- abs(diff(u))^(-gamma)
    m_v <- abs(diff(v))^(-gamma)
    
    w_u[which(w_u == Inf)] <- 999999
    w_v[which(w_v == Inf)] <- 999999
 
    Yk <- Y - ( X %*% (C - (u %*% t(v) * d)) )
    
    layers[[i]] <- list(Yk=Yk, u0=u, v0=v, d0=d, w_d=w_d, w_u=w_u, w_v=w_v, m_u=m_u, m_v=m_v)
  }
  return(layers)
}
