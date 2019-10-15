i1.min.u <- function(y,X,lam_l,lam_fu,w_v,w_u,w_d,m_u,v0,u0,d0,D,r,rho,z_thresh){

  #Set up to fit objective function
  Xu <- kron(as.matrix(v0), X)
  lamu <- (length(v0)^r)*lam_l*(sum(w_v*abs(v0)))
  
  
  uhat <- admmFAST(Xu,y,lam1=lamu,lam2=lam_fu,rho,u0,D,m_u,w_u*w_d,dtol=.001,ptol=.001,z_thresh,maxiter=10000)

  #Output uhat and dhat
  dhat <- norm(uhat,type="f")
  if (dhat == 0){
    dhat <- 900000000000
  }
  uhat <- setZero(uhat/dhat,z_thresh)
  
  
  return(list(dhat=dhat,uhat=uhat))
}
