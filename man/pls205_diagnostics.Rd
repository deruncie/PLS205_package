% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_diagnostics.R
\name{pls205_diagnostics}
\alias{pls205_diagnostics}
\title{pls205 diagnostics for linear models}
\usage{
pls205_diagnostics(model, EU = NULL)
}
\arguments{
\item{model}{A linear model fit with \code{lm} or \code{lmer}.}

\item{EU}{For models fit with \code{lmer}, the name of the Variable representing the Experimental Units.}
}
\value{
silently, a data.frame with columns for the fitted values, estimated EUs, and standardized residuals
}
\description{
Makes the diagnostics plots we use in PLS205:
qqplots and Scale-Location plots of experimental units
}
