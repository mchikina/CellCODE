\name{cellPopF}
\alias{cellPopF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign cell population using interaction F statistic
}

\usage{
cellPopF(data, grp, SPVs, intCheckPval = 0.1, useAll = F)
}

\arguments{
  \item{data}{
Gene expression data
}
  \item{grp}{
Clinical group factors.
}
  \item{SPVs}{
A matrix of cell proportion variables with a row for each sample of data and a column for each cell type.
}



  \item{intCheckPval}{	
P-value for interaction terms that do not match the direction of the group effect. If the interaction is bellow this p-value the gene cell population combination will receive a score of 0.
Can be set to 1 do not do any interaction checking.
}
  \item{useAll}{
Fit a interaction model using all other SPVs as covariates. Not recommended for smaller sample sizes.
}
}

\value{
A gene by cell-type matrix of scores. Scores are interaction F statistics (with some possibly zeroed out).

}

