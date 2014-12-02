isRowmax <-
function(data){
  datam=matrix(0, nrow=nrow(data), ncol=ncol(data));
  for (i in 1:nrow(data)){
    mm=max(data[i,], na.rm=T)
    if(mm>0){
      datam[i,]=(data[i,]==mm)
    }
  }
  rownames(datam)=rownames(data)
  datam
}
