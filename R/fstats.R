fstats <-
function (dat, mod, mod0=cbind(rep(1,ncol(data)))) 
{
  n <- dim(dat)[2]
  m <- dim(dat)[1]
  df1 <- dim(mod)[2]
  df0 <- dim(mod0)[2]
  p <- rep(0, m)
  Id <- diag(n)
  resid <- dat %*% (Id - mod %*% solve(t(mod) %*% mod) %*% 
                      t(mod))
  resid0 <- dat %*% (Id - mod0 %*% solve(t(mod0) %*% mod0) %*% 
                       t(mod0))
  rss1 <- resid^2 %*% rep(1, n)
  rss0 <- resid0^2 %*% rep(1, n)
  fstats <- ((rss0 - rss1)/(df1 - df0))/(rss1/(n - df1))
  return(fstats)
}
