tagData <-
function(data, cutoff=2, max=NULL, ref=NULL, ref.mean=F){
  
  if (! is.null(ref)){
    cm=intersect(rownames(data), rownames(ref))
    data=data[cm,]
    message(paste("There are ", length(cm), " common genes"))
  }
  dataTag=matrix(nrow=nrow(data), ncol=ncol(data))
  rownames(dataTag)=rownames(data)
  for (i in 1:(ncol(data))){
    mm=apply(data[, -i],1, max)
    dataTag[,i]=(data[,i]>cutoff+mm)*data[,i]    
  }
  if(! is.null(ref) & ref.mean){
    message(paste("Using reference data to select top genes"))
    mm=apply(ref,1,mean)
    for (i in 1:(ncol(data))){
      nn=rownames(data)[(which(dataTag[,i]>0))]
      dataTag[nn,i]=mm[nn]
    }
  }
  if (! is.null(max)){
    for ( i in 1:ncol(data)){
      cutoff = sort(dataTag[, i], decreasing = T)[max]
      countall=length(which(dataTag[, i] != 0))
      dataTag[which(dataTag[, i] < cutoff), i] = 0
      message(paste("selected",     length(which(dataTag[, i] != 0)), "out of", countall ))
      
    }
  }
  mm=rowSums(dataTag)
  dataTag=dataTag[mm>0,]
  colnames(dataTag)=colnames(data)
  dataTag
}
