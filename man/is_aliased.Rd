% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Variable_relationships.R
\name{is_aliased}
\alias{is_aliased}
\title{Checks pairs of Variables for a aliased relationship}
\usage{
is_aliased(term1, term2, data)
}
\arguments{
\item{term1, }{One Variable name of interest.}

\item{term2}{A second Variable name of interest.}

\item{data}{data.frame with columns (minimally) \code{term1} and \code{term2}}
}
\value{
TRUE or FALSE
}
\description{
Given two variable names and a data.table, checks if the \code{term1} is aliased with the \code{term2}.
Two terms are \emph{aliased} if there levels match 1:1.
Each level of \code{term1} is paired with exactly 1 level of \code{term2}.
}
