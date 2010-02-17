\name{gtkDfEditSetColumnClickHandler}
\alias{gtkDfEditSetColumnClickHandler}
\title{Function to call when column is clicked...}
\usage{gtkDfEditSetColumnClickHandler(object, columnClickHandler)}
\description{Function to call when column is clicked}
\details{IF set to NULL, no handler is called.}
\arguments{\item{object}{The RGtk2DfEdit object}
\item{columnClickHandler}{Function to call when column clicked. Signature is (dataframe, column number). If NULL (default)
no handler is called.}}
