% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backtesting.R
\name{getBacktestResults.Strategy}
\alias{getBacktestResults.Strategy}
\title{Returns aggregated results of backtests}
\usage{
\method{getBacktestResults}{Strategy}(this, recalc = TRUE)
}
\arguments{
\item{this}{Strategy}

\item{recalc}{bool, if true then it rerun calcBacktestResults function}
}
\value{
tibble
}
\description{
Returns aggregated results of backtests
}
