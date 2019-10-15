i2.lam.C <- function(X,lam_l,lam_fu,lam_fv,decomp,D,r,rho,tol=.01,maxit=1000,z_thresh=.001){
  

  uhat <- decomp$u0
  dhat <- decomp$d0
  vhat <- decomp$v0
  Chat <- dhat*uhat%*%t(vhat)
  y <- c(decomp$Yk)
  
  for (i in 1:maxit){
    Cprev <- Chat
    minv <- i1.min.v(y,X,
                     lam_l,lam_fv,
                     decomp$w_v,decomp$w_u,decomp$w_d,decomp$m_v,
                     vhat,uhat, D$vD,
                     r,z_thresh,rho)
    vhat <- minv$vhat
    dhat <- minv$dhat
    
    if (Matrix::nnzero(vhat)==0){  #If vhat is 0 it makes Xu 0 and throws an error
      Chat <- matrix(0,nrow=ncol(X),ncol=ncol(decomp$Yk))
      break
    }
    
    minu <- i1.min.u(y,X,
                  lam_l,lam_fu,
                  decomp$w_v,decomp$w_u,decomp$w_d, decomp$m_u,
                  vhat,uhat,dhat, D$uD,
                  r,rho,z_thresh)
    uhat <- minu$uhat
    dhat <- minu$dhat
    Chat <- dhat * uhat %*% t(vhat)
    
    
    if (Matrix::nnzero(uhat)==0){  #If uhat is 0 it makes Xv= 0 and throws an error
      Chat <- matrix(0,nrow=ncol(X),ncol=ncol(decomp$Yk))
      break
    }
    
    #Stopping criteria
    check = norm(Chat-Cprev,type="F")/norm(Cprev,type="F")
      if (check < tol){
        break
      }

  }
  bic <- i1.bic(X,decomp$Yk,Chat,uhat,vhat)
  return(list(Chat = Chat,bic=bic,uhat=uhat,vhat=vhat,dhat=dhat))
}
