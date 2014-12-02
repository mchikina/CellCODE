resid <-
function (dat, lab) 
{
  if (is.null(dim(lab))) {
    mod = model.matrix(~1 + lab)
  }
  else {
    mod = lab
  }
  n = dim(dat)[2]
  Id = diag(n)
  resid = dat %*% (Id - mod %*% solve(t(mod) %*% mod) %*% t(mod))
}
