\name{simulateMixture}
\alias{simulateMixture}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a mixture dataset
}
\description{
Simulates a mixture dataset with optional cell-type specific differential expression.
}
\usage{
simulateMixture(pureData, targetVals, cellpop = -1, frac.genes = 0.1, fold.sd = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pureData}{
A  gene by cell type matrix of pure cell expression profiles in linear space.
}
  \item{targetVals}{
A  cell-type by sample matrix of proportions.
}
  \item{cellpop}{
The index of the the cell type whose expression profile will be altered.
}
  \item{frac.genes}{
Fraction of genes to be differentially expressed.
}
  \item{fold.sd}{
The standard deviation for the fold change distribution. The mean is 0.
}
}
\details{
The sample size of the dataset is determined by the number of columns (samples) in the targetVals matrix. If the cellpop variable is set this function will simulate 2 groups of equal size. The second group will be created with slightly altered pureData.
}
\value{
A list with:
data: the simulated expression data
vals: a vector same size as nrow(data) of fold change values. Most of these will be 0.
}
