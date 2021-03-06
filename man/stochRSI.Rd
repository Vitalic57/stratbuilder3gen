% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tech_analysis.R
\name{stochRSI}
\alias{stochRSI}
\title{Stochastic RSI Oscillator}
\usage{
stochRSI(
  x,
  nRSI = 14,
  nFastK = 14,
  nFastD = 3,
  nSlowD = 3,
  maType = "EMA",
  bounded = TRUE,
  smooth = 1,
  ...
)
}
\arguments{
\item{x}{numeric, series of data}

\item{nRSI}{numeric, rsi arg n}

\item{nFastK}{numeric, stoch arg}

\item{nFastD}{numeric, stoch arg}

\item{nSlowD}{numeric, stoch arg}

\item{maType}{character or function, stoch arg}

\item{bounded}{logical, stoch arg}

\item{smooth}{numeric, stoch arg}

\item{...}{args to stoch}
}
\value{

}
\description{
Stochastic RSI Oscillator
}
