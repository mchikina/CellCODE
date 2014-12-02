\name{cellPopC}
\alias{cellPopC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign cell population using correlation
}

\usage{
cellPopC(data, grp, SPVs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Gene expression data
}
  \item{grp}{
Clinical group factors. Used only to remove the group effect before correlation computation.
}
  \item{SPVs}{
A matrix of cell proportion variable with a row for each sample of data and a column for each cell type
}
}

\value{
A gene by cell-type matrix of correlation coefficients
}

