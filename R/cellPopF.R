cellPopF <-
function(data, grp, SPVs, intCheckPval=0.1, useAll=F){
  out=matrix(nrow=nrow(data), ncol=ncol(SPVs))
  colnames(out)=colnames(SPVs)
  rownames(out)=rownames(data)
  dataraw=data
  
  for (i in 1:ncol(SPVs)) {    
    
    if (!useAll){
      mod=model.matrix(~1+grp)      
      modC=model.matrix(~1+grp*SPVs[,i]) 
    }
    else{      
      mod=model.matrix(~1+grp+SPVs)
      modC=model.matrix(~1+grp*SPVs[,i]+SPVs[,-i]) 
    }  
    dimmod=ncol(modC)  
    resl=lm.coef(data, modC)
    out[,i]=fstats(data, modC,mod)
    if (intCheckPval<1){  
      iibad=(sign(resl$tstat[,2])!=sign(resl$tstat[,dimmod])&resl$pval[, dimmod]<intCheckPval)    
      out[iibad, i]=0
    }   
  }  
  attr(out, "intCheckPval")=intCheckPval
  attr(out, "useAll")=useAll
out	    
}
