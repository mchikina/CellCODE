cellPopC <-
function(data, grp, SPVs){
  
  out=matrix(nrow=nrow(data), ncol=ncol(SPVs))
  colnames(out)=colnames(SPVs)
  rownames(out)=rownames(data)
  data=resid(data, grp)
  for (i in 1:ncol(SPVs)) {
    out[,i]=cor(t(data), SPVs[,i])
  }
  out
}
