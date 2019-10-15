EEA <- function(X,Y,rank,start_l, stop_l, start_fu=start_l, stop_fu=stop_l,start_fv=start_l, stop_fv=stop_l, k, k_fu = k, k_fv = k,z_thresh=.0001){
  
  decomp <- decomp.layer(X,Y,rank)
  D <- make.D(X,Y)
  
  lams <- make.lam(start_l=start_l, stop_l=stop_l, start_fu=start_fu, stop_fu=stop_fu,start_fv=start_fv, stop_fv=stop_fv, k=k, k_fu = k_fu, k_fv = k_fv)
  
  C_lay <- vector("list",rank)
  u <-  matrix(NA,nrow=rank,ncol=length(decomp[[1]]$u0))
  d <- rep(0,rank)
  v <- matrix(NA,nrow=rank,ncol=length(decomp[[1]]$v0))
  BIC <- rep(0,rank)
  l <- matrix(NA,nrow=rank,ncol=length(lams))
  
  
  for (i in 1:rank){
    temp <- i3.layer.C(X,lams,decomp[[i]],D,z_thresh=z_thresh)
    C_lay[[i]] <- temp$C
    u[i,] <- temp$u
    d[i] <- temp$d
    v[i,] <- temp$v
    BIC[i] <- temp$BIC
    l[i,] <- cbind(lams[[1]][temp$lms[1]],lams[[2]][temp$lms[2]],lams[[3]][temp$lms[3]])
  }
  
  C <- Reduce('+',C_lay)
  
  return(list(C=C,u=u,d=d,v=v,bic=BIC,lambda=l,C_lay = C_lay))
}
