getAllSPVs <-
function(data, grp, dataTag, method=c("mixed", "raw", "residual", "SVA"), plot=F, mix.par=0.3){
  cm=intersect(rownames(data), rownames(dataTag))
  dataTag=dataTag[cm,, drop=F]
  if (is.null(dim(grp))) {
    grp = model.matrix(~1 + grp)
  }
  if(is.null(method)){
    method="mixed"
  }
  method=match.arg(method)

  
  #build all SPVs
  SPVs=matrix(nrow=ncol(data), ncol=0)
  
  vv=apply(data,1,var)
  iiuse=which(vv[cm]>0)
  dataTag=dataTag[iiuse,,drop=F]
  for (i in 1:ncol(dataTag)) {
    
    genes=rownames(dataTag)[(which(dataTag[,i]>0))]
    
    
    if (method=="residual"){
      datar=resid(data,grp)	
      svdres=svds(datar[genes,])
      sv=svdres$v[,1]
    }
    else if (method=="mixed"){     
      
      sv=svdIntGrp(data[genes,], grp, cutoff=mix.par)
      
      ppcell=f.pvalue(t(sv), grp)  
      message(paste(colnames(dataTag)[i],"f p.value=",ppcell))
      if(min (ppcell)<0.05){
        message(paste("cell proportions may not be constant in ", colnames(dataTag)[i] )) 
      }
      
    }
    else if (method=="SVA"){
      
      svdres=irwsva.build(data[genes,], grp, cbind(rep(1,ncol(data))),n.sv=1, B=5)
      sv=svdres$sv
    }
    
    else if (method=="raw"){
      svdres=svds(data[genes,])
      sv=svdres$v[,1]
      
      ppcell=f.pvalue(t(sv), grp)  
      message(paste(colnames(dataTag)[i],"f p.value=",ppcell))
    }
    
    cc=cor(sv, t(data[genes,]))
    
    if (mean(cc)<0){
      sv=-sv
    }
    
    SPVs=cbind(SPVs, sv)
  }
  # colnames(SPVs)=colnames(dataTag)
  
  if (plot){
    parlast=par(no.readonly=T)
    datatmp=rbind(data[rownames(dataTag),], t(SPVs))
    datatmp=resid(datatmp, grp)
    labGenes=c(apply(dataTag, 1, function(x){which(x>0)[1]}), rep(ncol(dataTag)+1, ncol(SPVs)))
    mycol=c(rainbow(ncol(dataTag))[sample(ncol(dataTag))], "black")
    
    
    corplot(datatmp, usecordist=T, ColSideColors=mycol[labGenes], labRow=c(rep("", nrow(dataTag)), colnames(dataTag)), density.info="none", cexRow=1, dendrogram="column", key=F)
    par(mfg=c(1,par()$mfcol[2]))
    legend("topright", fill=mycol, legend=colnames(dataTag), ncol=1, xpd=NA, cex=1, inset=-0.1)
    par(parlast)
  }
  colnames(SPVs)=colnames(dataTag)
  SPVs
  
}
