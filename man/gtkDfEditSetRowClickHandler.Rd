\name{gtkDfEditSetRowClickHandler}
\alias{gtkDfEditSetRowClickHandler}
\title{Function to call when row is clicked...}
\usage{gtkDfEditSetRowClickHandler(object, rowClickHandler)}
\description{Function to call when row is clicked}
\details{IF set to NULL, no handler is called.}
\arguments{\item{object}{The RGtk2DfEdit object}
\item{rowClickHandler}{Function to call when row clicked. Signature is (dataframe, row number). If NULL (default)
no handler is called.}}
