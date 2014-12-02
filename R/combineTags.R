combineTags <-
function(tag1, tag2){
  mm=apply(tag1,1,max)
  tag1=tag1[mm>0,]
  mm=apply(tag2,1,max)
  tag2=tag2[mm>0,]
  genes1=setdiff(rownames(tag1), rownames(tag2))
  genes2=setdiff(rownames(tag2), rownames(tag1))
  genes=c(genes1, genes2)
  res=matrix(ncol=ncol(tag1)+ncol(tag2), nrow=length(genes))
  rownames(res)=genes
  colnames(res)=c(colnames(tag1), colnames(tag2))
  res[]=0
  res[genes1, colnames(tag1)]=tag1[genes1,]
  res[genes2, colnames(tag2)]=tag2[genes2,]
  res
  
}
