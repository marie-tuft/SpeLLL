make.lam <- function(start_l, stop_l, start_fu=start_l, stop_fu=stop_l,start_fv=start_l, stop_fv=stop_l, k, k_fu = k, k_fv = k){
  lam_l <- c(exp(rev(seq(log(start_l),log(stop_l),length.out = k))),0)
  lam_fu <- c(exp(seq(log(start_fu),log(stop_fu),length.out = k_fu)))
  lam_fv <- c(exp(seq(log(start_fv),log(stop_fv),length.out = k_fv)))
  #lambda <- cbind(c(exp(rev(seq(log(start_l),log(stop_l),length.out = k)))),c(exp(seq(log(start_fu),log(stop_fu),length.out = k))))
     return(list(lam_l=lam_l,lam_fu=lam_fu,lam_fv=lam_fv))
}
