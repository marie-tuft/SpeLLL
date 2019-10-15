#This function generates rank 1 simulated data

r1.sim <- function(){
  test.utilde <- c(10,-10,8,-8,5,-5,rep(3,5),rep(-3,5),rep(0,34))
  test.u <- test.utilde/norm(as.matrix(test.utilde),type="f")
  test.vtilde <- c(10,9,8,7,6,5,4,3,rep(2,17),rep(0,75))
  test.v <- test.vtilde/norm(as.matrix(test.vtilde),type="f")
  test.c <- 50*test.u %*% t(test.v)
  test.y <- test.c + matrix(rnorm(nrow(test.c)*ncol(test.c)), nrow=nrow(test.c))
  test.x <- diag(rep(1,nrow(test.c)))
  return(list(X = test.x, Y = test.y, C=test.c, u=test.u, v=test.v))
}
