% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signal_classes.R
\name{Rule}
\alias{Rule}
\title{Rule constructor}
\usage{
Rule(
  type = "enter",
  block,
  pathwise = FALSE,
  position = NULL,
  position_const = NULL,
  price = NULL,
  on_success = NULL,
  reopen = FALSE,
  ...
)
}
\arguments{
\item{type}{character, 'exit' or 'enter'}

\item{block}{character, it is needed for combining bunch of rules in one namespace. For example
rule with type exit will be triggered only if it has the same block variable as entry rule.}

\item{pathwise}{logical, whether expr will be calculated on each iteration or not. In other words if pathwise is FALSE
then expr will be calculated once with indicators, it should return logical vector, then in each iteration element of this
vector will be used, if pathwise is TRUE then expr will be calculated at each iteration, it should return logical scalar.}

\item{position}{numeric vector, what position for each instrument should be at each iteration when model in position}

\item{position_const}{numeric vector, what position for each instrument should be at entry to position}

\item{price}{numeric vector, what prices should be used for change position}

\item{reopen}{logical, whether rule can be reopened on the same bar}

\item{...}{args passed to Signal}
}
\description{
This class serves for describing moments when model should open position or exit from it.
}
