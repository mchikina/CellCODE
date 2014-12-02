\name{getAllSPVs}
\alias{getAllSPVs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get surrogate proportion variables (SPVs)
}
\description{
Estimates SPVs from data and cell type markers.
}
\usage{
getAllSPVs(data, grp, dataTag, method = c("mixed", "raw", "residual", "SVA"), plot = F, mix.par = 0.3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A matrix of expression data.
}
  \item{grp}{
A factor of group assignments. Same size as ncol(data).
}
  \item{dataTag}{
A matrix of cell type markers as those produced by the tagData function. Genes are in rows and cell types are in columns. A non-zero value in row i column j indicates that a gene i is a marker for cell type j.
}
  \item{method}{
The method used to account for the group effect when estimating SPVs.
mixed: The suggested method. Attempts to figure out which genes are differentially expressed at the single cell level and remove those from SPV estimation.
raw: Does not account for group effect. Uses all genes as is.
residual: Group effect is removed before SPV estimations. SPVs will have equal within group means.
SVA: Same idea as mixed but uses the default SVA framework.

}
  \item{plot}{
Whether or not the results should be plotted as a correlation heatmap. 
}
  \item{mix.par}{
The fraction of genes to remove in the mixed method. The default parameter is usually suitable.
}
}

\value{
A sample by cell-type matrix of surrogate proportion variables.
}
