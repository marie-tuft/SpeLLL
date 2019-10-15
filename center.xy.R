#This function centers the columns of X and Y

center.xy <- function(Y,X=NULL){
  if (is.null(X)){
    return(apply(Y, 2, function(a) (a-mean(a))))
  } else {
    sY <- apply(Y, 2, function(a) (a-mean(a)))
    sX <- apply(X, 2, function(a) (a-mean(a)))
    sXY <- list(X=sX,Y=sY)
    return(sXY)
  }
}