i1.min.v <- function(y,X,lam_l,lam_fv,w_v,w_u,w_d,m_v,v0,u0,D,r,z_thresh,rho){
  
  #Set up to fit objective function
  Xv <- kron(diag(rep(1,length(v0))), X %*% as.matrix(u0))
  lamv <- (length(u0)^r)*lam_l*(sum(w_u*abs(u0)))
  
  #Solve using ADMM
  vtilde <- admmFAST(Xv,y,lam1=lamv,lam2=lam_fv,rho=1,initial=v0,D,m_v*w_d,w_v*w_d,dtol=.001,ptol=.001,z_thresh=z_thresh,maxiter=10000)
  
  #Output vhat and dhat
  #vtilde <- as.matrix(coef(fitv))[-1]
  dhat <- norm(vtilde, type = "f")
  if (dhat == 0){
    dhat <- 900000000000
  }
  vhat <- setZero(vtilde/dhat,z_thresh)

  return(list(dhat=dhat,vhat=vhat))
}


