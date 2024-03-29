% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signal_classes.R
\name{Indicator}
\alias{Indicator}
\title{Indicator constructor}
\usage{
Indicator(lookforward = Inf, vars = NULL, ...)
}
\arguments{
\item{lookforward}{numeric / expression. If it is numeric then after that number of points all indicators will be recalculated.
If it is expression then it should return logical. It will indicate recalculate indicators or not.}

\item{vars}{character vector, which names are defined in expr. This field is necessary for definition of indicators dependencies. 
Update of one indicator trigger another indicators for update.}

\item{...}{args passed to Signal}
}
\description{
Indicators serves for calculating rules
}
