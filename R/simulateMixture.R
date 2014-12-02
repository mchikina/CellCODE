simulateMixture <-
function(pureData, targetVals,  cellpop=-1, frac.genes=0.1, fold.sd=2){
  stopifnot(ncol(pureData)==nrow(targetVals))
  n=ncol(targetVals)
  ncon=n/2
  condata=pureData%*%targetVals[, 1:ncon]
  ngenes=nrow(pureData)
  vals=double(ngenes)
  if(cellpop!=-1){
    iidiff=sample(ngenes,frac.genes*ngenes)
    vals[iidiff]=(rnorm(length(iidiff),0,fold.sd))
    pureData[, cellpop]=pureData[, cellpop]*exp(vals)
  }
  diffdata=pureData%*%targetVals[, (ncon+1):n]
  data=cbind(condata, diffdata)
  names(vals)=rownames(data)
  return(list(data=data, vals=vals))
}
