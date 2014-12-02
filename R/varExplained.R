varExplained <-
function (dat, mod, adjust=T) 
{
  
  
  mod0 = cbind(rep(1, ncol(dat)))
  
  n=ncol(dat)
  if(adjust){
    adj=(n-1)/(n-ncol(mod))
  }
  else{
    adj=1
  }
  n=ncol(dat)
  resid = resid(dat, mod)
  resid0 = resid(dat, mod0)
  rss1 = resid^2 %*% rep(1, n)
  rss0 = resid0^2 %*% rep(1, n)
  
  return(1-rss1/rss0*adj)
}
