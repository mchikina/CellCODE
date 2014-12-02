\name{combineTags}
\alias{combineTags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combines cell type marker matricies
}
\description{
Combines two cell type marker matricies (such as those produced by tagData()) into one matrix with a common gene list. Genes that were markers for two different cell types (columns) are removed. Genes that do not mark any celltype are also removed.
Can be used to combine tags from different datasets of pure cell types.
}
\usage{
combineTags(tag1, tag2)
}

