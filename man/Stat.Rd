% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Stat.R
\name{Stat}
\alias{Stat}
\title{Class for one statistic}
\usage{
Stat(description, in_report, func, keywords, depends, name, general)
}
\arguments{
\item{description}{character description of stat}

\item{in_report}{logical if TRUE then it will can be accessed in report}

\item{func}{function how your stats should be calculated, this function should has three arguments

* this - Strategy object

* start - index of starting period of backtest

* end - index of ending period of backtest}

\item{keywords}{character additional names for your stat}

\item{depends}{character names of other defined stats, they will be calculated before this stat}

\item{name}{character name that will be used in report}

\item{general}{logical, if TRUE then stat will be calculated once for all periods}
}
\value{
Stat object
}
\description{
Class for one statistic
}
