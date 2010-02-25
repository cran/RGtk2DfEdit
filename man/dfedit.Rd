\name{dfedit}
\alias{dfedit}
\title{Convenience function for editing a data frame in its own window}
\usage{dfedit(items, dataset.name = deparse(substitute(items)), 
size=c(500, 300), col.width=64)}                 
\description{Optional name to assign the dataset when updating}
\details{IF set to NULL, is the name of the dataset.}
\value{Returns the \code{\link{gtkDfEdit}} object}
\arguments{\item{items}{The data frame to edit}
\item{dataset.name}{Optional name to assign the dataset when updating}
\item{size}{Optional window size request}
\item{col.width}{Optional column size}}
\examples{ 
  obj <- dfedit(iris)
}
