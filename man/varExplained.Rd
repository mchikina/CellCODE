\name{varExplained}
\alias{varExplained}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reports the fraction of variance explained by the model for each measurement in dat.
}

\usage{
varExplained(dat, mod, adjust = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A matrix with measurements in rows and samples in columns
}
  \item{mod}{
A model such as that produced by model.matrix
}
  \item{adjust}{
Adjust the calculation for model size. Will produce some negative numbers but allows cross-model comparison.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length nrow(dat) with fraction variance explained for each measurement in dat.

}
