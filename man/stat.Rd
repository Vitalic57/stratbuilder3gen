% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genericStrategy.R, R/parameters.R
\name{addStat}
\alias{addStat}
\alias{removeStat}
\alias{reinitStat}
\alias{addStat.Strategy}
\alias{removeStat.Strategy}
\alias{reinitStat.Strategy}
\alias{updateStat}
\alias{updateStat.Strategy}
\alias{extractStat.Strategy}
\alias{extractStat}
\title{This variable will be included to program and after calling perform function  it will be updated}
\usage{
addStat(this, ...)

removeStat(this, name)

reinitStat(this)

\method{addStat}{Strategy}(this, ...)

\method{removeStat}{Strategy}(this, name)

\method{reinitStat}{Strategy}(this)

updateStat(this, e)

\method{updateStat}{Strategy}(this, e)

\method{extractStat}{Strategy}(this, e)

extractStat(this, e)
}
\arguments{
\item{this}{Strategy}

\item{...}{params}

\item{name}{character}

\item{e}{environment}
}
\description{
This variable will be included to program and after calling perform function  it will be updated

Deletes variable from stats

Reinit all variables in stats

Update all variables in stats from environment

Update environment with user-defined stats
}
