i3.layer.C <- function(X, lambda, decomp, D, r=-1, rho=1,tol=.01,maxit=1000,z_thresh=.001) {

  bic <- 100000

  for (j in 1:length(lambda[[2]])){

    for (k in 1:length(lambda[[3]])){
    
    message(paste("outer=",j))  
   
    decomp2 <- decomp 
    

    for (i in 1:length(lambda[[1]])){
      
      fit <- i2.lam.C(X,lambda[[1]][i],lambda[[2]][j],lambda[[3]][k],decomp2,D,r,rho,tol,maxit,z_thresh)
      
      #only takes the lowest bic so I don't have to save k^2 chats and bics
      if (fit$bic < bic){
        Chat <- fit$Chat
        bic <- fit$bic
        uhat <- fit$uhat
        vhat <- fit$vhat
        dhat <- fit$dhat
        lms <- c(i,j,k)
      }
      

      
      if (Matrix::nnzero(fit$uhat)==0|Matrix::nnzero(fit$vhat)==0){
      next
      } else {
        decomp2$u0 <- fit$uhat
        decomp2$v0 <- fit$vhat
        decomp2$d0 <- fit$dhat
      }
    }
    }
  }
  
  return(list(C=Chat,lms=lms,BIC=bic,u=uhat,v=vhat,d=dhat))
  
}
