#This function generates a rank 3 simulated data set

r3.sim <- function(){
  
  Au <- function(n){
    p <- rbinom(n,1,.5)
    out <- (p*runif(n, min=-1,max=-.3)) + ((1-p)*runif(n,min=.3,max=1))
    return(out)
  }
  
  t2.d <- c(20,10,5)
  t2.u <- cbind(c(Au(5),rep(0,20)),  c(rep(0,5),Au(5),rep(0,15)), c(rep(0,10),Au(5),rep(0,10)))
  t2.v <- cbind(c(runif(10,-1,1),rep(0,15)), c(rep(0,12),runif(10,-1,1),rep(0,3)))
  t2.v<- cbind(t2.v, c(rep(0,6),t2.v[7:8,1],-t2.v[9:10,1],runif(2,-1,1),-t2.v[13:14,2],t2.v[15:16,2],rep(0,9)))
  
  t2.u <- apply(t2.u,2, function(a) a/norm(as.matrix(a),type="f"))
  t2.v <- apply(t2.v,2, function(a) a/norm(as.matrix(a),type="f"))
  
  C1 <- (t2.d[1] * t2.u[,1] %*% t(t2.v[,1]))
  C2 <- (t2.d[2] * t2.u[,2] %*% t(t2.v[,2]))
  C3 <- (t2.d[3] * t2.u[,3] %*% t(t2.v[,3]))
  
  t2.c <- C1 + C2 + C3
  
  
  sigma <- matrix(.5, nrow=25, ncol=25)
  exp <- which(sigma == .5, arr.ind=TRUE)
  exp <- matrix(abs(exp[,1]-exp[,2]),nrow = 25)
  sigma <- sigma^exp
  
  t2.X <- MASS::mvrnorm(n=1, mu=rep(0,25),Sigma=sigma)
  for (i in 1:49){
    t2.X <- rbind(t2.X,MASS::mvrnorm(n=1, mu=rep(0,25),Sigma=sigma))
  }
  
  t2.Y <- (t2.X %*% t2.c) + matrix(rnorm(50*25),nrow=50,ncol=25)
  
  return(list(X=t2.X, Y=t2.Y, C=t2.c, C1=C1, C2=C2, C3=C3, u=t2.u, d=t2.d, v=t2.v))
}
