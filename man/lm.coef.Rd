\name{lm.coef}
\alias{lm.coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple linear model fitting
}

\usage{
lm.coef(dat, mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix of data such as gene expression data.
}
  \item{mod}{
A model matrix.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of result matricies with dimensions nrow(dat) ncol(mod)
tstat: T statistics for the coefficients
pval: p-values for the coefficients
coeff: raw coefficients
se: standard error 
}


