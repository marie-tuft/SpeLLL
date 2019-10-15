smse <- function(C,Chat){
  return(100*norm(C-Chat,"F")/(nrow(C)*ncol(C)))
}
