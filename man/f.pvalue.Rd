\name{f.pvalue}
\alias{f.pvalue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
F p-values
}
\description{
A simple function to compute F test p-values for every row of a matrix.
}
\usage{
f.pvalue(dat, lab, mod0 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix of data
}
  \item{lab}{
Either a factor variable or a model matrix.
}
  \item{mod0}{
If lab is a model matrix this is used as the null model.
}
}
