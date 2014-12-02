svdIntGrp <-
function(dat, grp, cutoff=0.3){
  pp=f.pvalue(dat, grp)
  datr=resid(dat, grp)
  iismall=order(pp)[1:floor(nrow(dat)*cutoff)]
  for (i in 1:2){
    svdres=svds(dat[-iismall,])
    sv=svdres$v[,1, drop=F]
    mod=cbind(grp, sv, sv*grp[,2])
    mod0=cbind(grp[,1], sv)
    pp=f.pvalue(dat[,], mod, mod0)
    iismall=order(pp)[1:floor(nrow(dat)*cutoff)]
  }
  
  svdres=svds(dat[-iismall,])
  return(cbind(svdres$v[,1, drop=F]))
}
