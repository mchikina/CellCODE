f.pvalue <-
function (dat, lab, mod0 = NULL) 
{
  if (is.null(mod0)) {
    mod0 = cbind(rep(1, ncol(dat)))
  }
  if (is.null(dim(lab))) {
    mod = model.matrix(~1 + lab)
  }
  else {
    mod = lab
  }
  n = dim(dat)[2]
  m = dim(dat)[1]
  df1 = dim(mod)[2]
  df0 = dim(mod0)[2]
  p = rep(0, m)
  Id = diag(n)
  resid = resid(dat, mod)
  resid0 = resid(dat, mod0)
  rss1 = resid^2 %*% rep(1, n)
  rss0 = resid0^2 %*% rep(1, n)
  fstats = ((rss0 - rss1)/(df1 - df0))/(rss1/(n - df1))
  p = 1 - pf(fstats, df1 = (df1 - df0), df2 = (n - df1))
  return(p)
}
