numAtFDR <-
function(labels, vals, at=0.2){
  TP=cumsum(labels[order(vals, decreasing=T)])/(1:length(vals))
  ii=which(TP<(1-at))
  if (length(ii)>0){
    return(as.numeric(ii[1]-1))
  }
  else{
    return(0)
  }  
}
