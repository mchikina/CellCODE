corplot <-
function(data, usecordist=T, ...){
  par(omi=c(1,0,0,1))
  cc=cor(t(data))
  col=colorpanel(21, "green", "white", "red")
  
  if (usecordist){
    dd=as.dendrogram(hclust(as.dist(1-cc), method="average"))
    heatmap.2(cc, col=col, symbreaks=T, trace="none", Rowv=dd, Colv=dd, ...)
  }
  else{
    heatmap.2(cc, col=col, symbreaks=T, trace="none", ...)
    
  }
}
