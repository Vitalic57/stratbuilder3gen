% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericStrategy.R, R/reports.R
\name{calcStat_}
\alias{calcStat_}
\alias{calcStat_.Strategy}
\title{Calculate statistic}
\usage{
calcStat_(this, s, start, end, args)

\method{calcStat_}{Strategy}(this, s, start, end, args = list())
}
\arguments{
\item{this}{Strategy}

\item{s}{Stat}

\item{start}{numeric, start of the period}

\item{end}{numeric, end of the period}

\item{args}{list, list of arguments for function in s}
}
\description{
Statistic and all dependent statistics will be calculated
}
