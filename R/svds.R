svds <-
function(data){
  mm=apply(data,1,mean)
  tmp=sweep(data,1,mm, "-")
  svd(tmp)
}
