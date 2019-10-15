make.D <- function(X,Y){

u <- ncol(X)
v <- ncol(Y)
  
uD <- diag(1,nrow=u)
lD <- matrix(nrow = u-1, ncol = u)
lD <- ((row(lD) == col(lD) -1) + 0) - diag(1, nrow=nrow(lD), ncol = ncol(lD))
u_D <- rbind(uD,lD)

uD <- diag(1,nrow=v)
lD <- matrix(nrow = v-1, ncol = v)
lD <- ((row(lD) == col(lD) -1) + 0) - diag(1, nrow=nrow(lD), ncol = ncol(lD))
v_D <- rbind(uD,lD)

return(list(uD=u_D, vD=v_D))
}

