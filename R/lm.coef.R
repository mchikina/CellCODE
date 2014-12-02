lm.coef <-
function(dat, mod){
  df=ncol(dat)-ncol(mod)
  xx=solve(t(mod) %*% mod) %*% t(mod)
  coeff=xx %*%t(dat)
  n=ncol(dat)
  Id = diag(n)
  resid = dat %*% (Id - mod %*% solve(t(mod) %*% mod) %*%
                     t(mod))
  
  rss = resid^2 %*% rep(1, n)
  
  xx=solve(t(mod) %*% mod)
  myse=sqrt(rss)%*%t(sqrt(diag(xx)/df))
  ts=t(coeff)/myse
  rownames(ts)=rownames(dat)
  colnames(ts)=colnames(mod)
  pp=2*pt(abs(ts), df=df, lower.tail=F)
  return(list(tstat=ts, pval=pp, coeff=t(coeff), se=myse))
}
