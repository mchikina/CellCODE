\name{corplot}
\alias{corplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot correlation structure.
}
\description{
Produces a heatmap plot of all pairwise correlations. The colors are green to white to red with white being 0 correlation.
}
\usage{
corplot(data, usecordist = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A matrix of data such as gene expression. Correlation among all pairs of rows will be plotted.
}
  \item{usecordist}{
Use correlation for the distance measure for heatmap clustering. Otherwise dist is uses.
}
  \item{\dots}{
Other parameters to be passed to heatmap.2, for example RowSideColors.
}
}
