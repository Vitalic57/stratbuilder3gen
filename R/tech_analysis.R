#' Stochastic RSI Oscillator
#'
#' @param x numeric, series of data
#' @param nRSI numeric, rsi arg n
#' @param nFastK numeric, stoch arg
#' @param nFastD numeric, stoch arg
#' @param nSlowD numeric, stoch arg
#' @param maType character or function, stoch arg
#' @param bounded logical, stoch arg
#' @param smooth numeric, stoch arg
#' @param ... args to stoch
#'
#' @return
#' @export
stochRSI <- function(x, nRSI = 14, nFastK = 14, nFastD = 3, nSlowD = 3, maType = 'EMA', bounded = TRUE,
                     smooth = 1, ...){
  tmp <- RSI(x, n = nRSI, maType = maType, ...)
  tmp <- stoch(tmp, nFastK = nFastK, nFastD = nFastD, nSlowD = nSlowD, maType = maType, bounded = bounded,
               smooth = smooth, ...)
  return(tmp)
}


#' Varadi's MSR
#'
#' Computes David Varadi's MSR – a percent rank of a normalized differnce of median and max
#'
#' @param HLC an HLC xts
#' @param nMed a lookback period for taking the median of the HLC series; i.e. the median of the concatenated HLC series, using a parameter of 3*nMed for the 3 series in the concatenation
#' @param nMax a lookback period for the max of the HLC series – should be greater than the median lookback
#' @param pctLookBack a period over which to do a percent ranking
#'
#' @return
#' @export
MSR <- function (HLC, nMed = 10, nMax = nMed * 2, pctLookBack = 100) {
  HLC <- try.xts(HLC, error = as.matrix)
  if (NCOL(HLC) == 3) {
    high <- HLC[, 1]
    low <- HLC[, 2]
    close <- HLC[, 3]
  }else if (NCOL(HLC) == 1) {
    high <- HLC
    low <- HLC
    close <- HLC
  }
  dates <- Sys.Date() + 1:nrow(HLC)
  HLConeSeries <- rbind(xts(high, dates), xts(low, dates), xts(close, dates))
  HLCrunMed <- runMedian(HLConeSeries, n = nMed * 3)
  med <- coredata(HLCrunMed)[seq(1, nrow(HLC) * 3, 3),]
  runmax <- runMax(high, nMax)
  tmp <- (med - runmax)/runmax
  out <- cbind(runPercentRank(tmp, n = pctLookBack, exact.multiplier = 1))
  colnames(out) <- "MSR"
  out
}

# CCI
# MADBBands <- function(x, n = 20, sd = 1, ...){
#   m <- RollingWindow::RollingMedian(x, window = n, ...)
#   me <- abs(x - m)
#   dev <- RollingWindow::RollingMedian(me, window = n, ...)
#   return(cbind(m - dev*sd, m, m + dev*sd, (x - m + dev * sd) / 2 / dev / sd ) %>% set_colnames(c('dn','mavg','up', 'pctB')))
# }


#' The Triple Exponential Moving Average (T3) by Tim Tillson
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#'
#' @export
#' @rdname MA
T3MA <- function(x, n=10, vFactor=0.7){
  GD <- function(x, n, vFactor){
    EMA(x, n) * (1 + vFactor) - EMA(EMA(x, n), n) * vFactor
  }
  GD(GD(GD(x, n, vFactor), n, vFactor), n, vFactor)
}

#' Triple exponential moving average
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#'
#' @export
#' @rdname MA
TEMA <- compiler::cmpfun(function(x, n=10){
  DEMA(x, n) + EMA(x - DEMA(x, n), n)
})

#' Triangular Moving Average
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#'
#' @export
#' @rdname MA
TRIMA <- function(x, n=10){
  N <- floor((n + 1) / 2)
  SMA(SMA(x, N), N)
}

#' Variable Index Dynamic Average
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#'
#' @export
#' @rdname MA
VIDYA <- function(x, n){
  N <- length(x)
  s <- rep(NA, N)
  if (N > 1){
    f <- 2 / (n + 1)
    diffs <- Diff(x)
    upSums <- RollingWindow::RollingSum(pmax(diffs, 0), window = n , na_method = 'ignore')
    dnSums <- RollingWindow::RollingSum(pmax(-diffs, 0), window = n , na_method = 'ignore')
    absCMO <- abs((upSums - dnSums) / (upSums + dnSums))
    s[n] <- mean(x[1:n], na.rm = TRUE)
    for(i in (n + 1):N){
      s[i] <- f * absCMO[i] * x[i] + (1 - f * absCMO[i]) * s[i-1]
    }
  }
  return(s)
}

#' (Kaufmann)  Adaptive Moving Average
#'
#' @param x Price, volume, etc. series that is coercible to xts or matrix.
#' @param n numeric Number of periods to average over. Must be between 1 and nrow(x), inclusive.
#' @param FC numeric min period to average
#' @param SC numeric max period to average
#' @param power numeric power of adaptive weight by default it is 1, recommended value is 2
#'
#' @export
#' @rdname MA
AMA <- function(x, n = 10, FC = 1, SC = 200, power=1){
  N <- length(x)
  s <- rep(NA, N)
  if (N > 1){
    diffs <- Diff(x)
    signal <- abs(Diff(x, n))
    noise <- RollingWindow::RollingSum(abs(Diff(x)), window = n, na_method = 'ignore')
    SSC <- (signal / noise * ( 2 / (FC + 1) - 2 / (SC + 1)) + 2 / (SC + 1)) ^ power
    s[n] <- mean(x[1:n], na.rm = TRUE)
    for(i in (n + 1):N){
      s[i] <- SSC[i] * x[i] + (1 - SSC[i]) * s[i-1]
    }
  }
  return(s)
}
