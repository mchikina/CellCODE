\name{tagData}
\alias{tagData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds cell type markers for a matrix of pure cell type expression data
}

\usage{
tagData(data, cutoff = 2, max = NULL, ref = NULL, ref.mean = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Pure cell expression data. Each column represents a unique cell type.
}
  \item{cutoff}{
In order to be considered a marker gene the gene's expression in a single cell type must exceed all other cell types by this amount.
}
  \item{max}{
The maximum number of marker genes to return. The most highly expressed genes are picked from the set of eligible genes.
}
  \item{ref}{
A reference dataset. This dataset is used to filter the set of genes so that every marker gene is present in the reference dataset
}
  \item{ref.mean}{
Use the reference dataset to define genes expression levels. With this option which marker genes are returned (if max is set to a number) is based on their expression in the reference dataset and not the pure cell dataset.
Can be useful if the absolute expression levels in the pure cell dataset are unreliable.
}
}
