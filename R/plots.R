#' Plot change of profit and loss through backtest period
#'
#' @param this Strategy
#' @param type character, one of c('money','trades','percents')
#' @param leg numeric/character, number or numbers of legs, if it is equal to 'all' or 'sum', then all pnl among all legs
#' will be summed, if it is equal to 'sep', then pnl among legs will be plotted
#' @param graph_type character, ggplot2 or xts
#' @param each_year logical, if TRUE, then each graph will start with 0 each year
#' @param adjust logical, if TRUE, then values will be divided by getMoney(this)
#' @param ... params
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param return_type character, plot or data
#' @param cutoff logical, if true then vertical line will plotted on graph where model was created
#'
#' @export
#' @rdname plotPnL
#' @method plotPnL Strategy
plotPnL.Strategy <- function(this,
                             type = 'money',
                             comOn = TRUE,
                             leg = 'all',
                             graph_type = 'ggplot2',
                             each_year = FALSE,
                             adjust = FALSE,
                             return_type = 'plot',
                             cutoff = FALSE,
                             start,
                             end,
                             ...){
  e <- this$backtest
  legs <- leg
  leg <- legs[1]
  switch(type,
         money = {
           dates <- getDateByIndex(this)
           dots <- list(...)
           range_start <- get_backtest_start_index(this, start)
           range_end <- get_backtest_end_index(this, end)
           if(range_start > range_end){
             stop("start > end")
           }
           range <- range_start:range_end
           init_money <- getMoney(this)  #e$results$money[range_start,]
           if(leg %in% c('all', 'sum')){
             df <- cbind(
               data.frame(date=dates),
               data.frame(PnL = init_money + calcStat(this, acceptable_stats$pnl_, range_start, range_end) +
                            rowSums(apply( (1 - comOn) * e$results$commissions_table, 2, cumsum)))
             )[range,]
           }else if(is.numeric(leg)){
             leg <- legs
             df <- cbind(
               data.frame(date=dates),
               data.frame(PnL = init_money + e$results$unrealized_money[,leg] + e$results$realized_money[,leg] +
                            cumsum((1 - comOn) * e$results$commissions_table[, leg]))
             )[range,]
           }else if(leg %in% c('sep', 'separate', TRUE)){
             leg <- 'sep'
             df <- cbind(
               data.frame(date=dates),
               data.frame(init_money + e$results$unrealized_money + e$results$realized_money +
                            apply( (1 - comOn) * e$results$commissions_table, 2, cumsum)) %>%
                 set_colnames(colnames(getModelD(this)$data_raw))
             )[range,]
           }
           if(adjust){
             df[,-1] <- df[,-1] / init_money - 1
           }
           if(each_year){
             if(!adjust){
               df[,-1] <- df[,-1] - init_money
             }

             tmp <- xts(df[,-1], df[,1]) %>% set_colnames(colnames(df)[-1])

             #last_dates <- apply.yearly(tmp, FUN = nrow ) %>% as.numeric %>% cumsum
             last_dates <- apply.yearly(tmp, FUN = function(x) tail(x, 1) %>% index ) %>% as.numeric %>% head(-1)

             df <- apply.yearly(tmp, FUN = function(x) sweep(x, 2, x[1,]) ) %>%
               {
                 res <- list()
                 for(i in 1:length(.)){
                   res[[i]] <- .[[i]]
                 }
                 res
               } %>%
               Reduce('rbind', .) %>%
               coredata %>%
               as.data.frame %>%
               set_colnames(colnames(tmp)) %>%
               dplyr::mutate(date = df[, 'date'])
           }
           if(return_type == 'plot'){
             if(graph_type == 'ggplot2'){
               newdf <- reshape2::melt(df, 'date')
               p <- ggplot2::ggplot(newdf, ggplot2::aes(x=date, y=value, color = variable) ) +
                 ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("PnL money by date")
               if(each_year){
                 p <- p + ggplot2::geom_vline(xintercept=last_dates, linetype=4, colour="red")
               }
               if(cutoff && 'created' %in% names(this$thisEnv)){
                 p <- p + ggplot2::geom_vline(xintercept=as.numeric(this$thisEnv$created), linetype=4, colour="green")
               }
               if(leg != 'sep'){
                 p + ggplot2::scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + ggplot2::theme(legend.position="none")
               }else{
                 p
               }


             }else{
               ind <- which(colnames(df) == 'date')
               plot(xts(df[,-ind], df[, ind]), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
             }
           }else if(return_type == 'data'){
             ind <- which(colnames(df) == 'date')
             return(xts(df[,-ind], df[, ind]))
           }


         },
         trade =,
         trades =,
         money_trades = ,
         money_trade =,
         trades_money = ,
         trade_money = {
           report <- getReportTrades(this,  ...)
           init_money <- e$results$money[e$activeField['start'],]
           if(leg %in% c('all', 'sum')){
             tmp <- report$pnl.sum
             if(comOn){
               tmp <- tmp - report$com.sum
             }
             pnl <- cumsum(c(0,tmp)) + init_money
           }else if(is.numeric(leg)){
             ind_pnl <- which(grepl('pnl.asset', colnames(report)))
             ind_com <- which(grepl('com.asset', colnames(report)))
             pnl <- cumsum(c(0, report[, ind_pnl[leg]] - comOn * report[, ind_com[leg]]))
           }else if(leg %in% c('sep', 'separate')){
             ind_pnl <- which(grepl('pnl.asset', colnames(report)))
             ind_com <- which(grepl('com.asset', colnames(report)))
             pnl <- rbind(rep(0, length(ind_pnl)) , report[, ind_pnl] - comOn * report[, ind_com]) %>%
               apply(2, cumsum)
           }

           if(leg == 'sep'){
             df <- data.frame(pnl) %>%
               set_colnames(colnames(getModelD(this)$data_raw)) %>%
               dplyr::mutate(index = 1:nrow(pnl))
           }else{
             df <- data.frame(PnL = pnl, index = 1:length(pnl))
           }

           if(return_type == 'plot'){
             if(graph_type == 'ggplot2'){
               newdf <- reshape2::melt(df,'index')
               p <- ggplot2::ggplot(newdf, ggplot2::aes(x= index, y = value, color = variable) ) +
                 ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("PnL money by trade")
               if(leg != 'sep'){
                 p + ggplot2::scale_color_manual(
                   values = c(
                     PnL = 'darkblue'
                   )) + ggplot2::theme(legend.position="none")
               }else{
                 p
               }

             }else{
               plot(df[,-ncol(df)], type = 'l', main = 'PnL', ylab = 'money', xlab = 'trades')
             }
           }else if(return_type == 'data'){
             return(xts(df[,-ncol(df)], getDateByIndex(this, as.numeric(report[,'end.ind']))))
           }

         }
  )
}

#' Plot drawdowns
#'
#' @param this Strategy
#' @param from character, name of backtest
#' @param return_type character, plot or data
#' @param graph_type character, ggplot2 or xts
#' @param ... params
#'
#' @return ggplot/xts
#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns Strategy
plotDrawdowns.Strategy <- function(this,
                                   return_type = 'plot',
                                   graph_type = 'ggplot2',
                                   ...){
  e <- this$backtest
  dates <- getDateByIndex(this)
  range_start <- e$activeField['start']
  range_end <- e$activeField['end']
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  df <- cbind(
    data.frame(date=dates),
    data.frame(PnL = e$results$money - cummax(e$results$money))
  )[range,]
  if(return_type == 'plot'){
    if(graph_type == 'ggplot2'){
      newdf <- reshape2::melt(df, 'date')
      ggplot2::ggplot(newdf,aes(x=date, y=value, color = variable) ) +
        ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::theme(legend.position="none") +
        ggplot2::scale_color_manual(
          values = c(
            PnL = 'darkblue'
          ))+
        ggplot2::ggtitle("Drawdowns by date")
    }else{
      plot(xts(df[,'PnL'], df[,'date']), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
    }
  }else if(return_type == 'data'){
    return(xts(df[,'PnL'], df[,'date']))
  }
}

#' Plot returns vs MAE/MFE
#'
#' @param this modelStrategy
#' @param type character, MAE or MFE
#' @param from character, name of backtest
#'
#' @return ggplot
#' @export
#' @rdname plotReturns
#' @method plotReturns Strategy
plotReturns.Strategy <- function(this, type = 'MAE'){
  e <- this$backtest
  report <- getReportTrades(this) %>%
    dplyr::mutate(ind = 1:dplyr::n())
  rets <- 'pnl.sum.adj'
  switch(type,
         MAE=,
         min=,
         loss=,
         maxloss={
           var <- 'MAE.with.com'
         },
         MFE=,
         max=,
         profit=,
         maxprofit={
           var <- 'MFE.with.com'
         }
  )

  df <- report[,c(rets,var)] %>%
    set_colnames(c('rets','var'))
  p <- ggplot2::ggplot(df, aes(abs(rets),var, group = rets < 0 )) +
    ggplot2::geom_point(aes(col = rets < 0, shape = rets  < 0) , size = 3) +
    #geom_point(aes(colour = rets > 0)) +
    #scale_shape(solid = FALSE) +
    ggplot2::scale_shape_manual(values=c(24,25)) +
    ggplot2::scale_color_manual(values=c('green','red')) +
    ggplot2::geom_abline(intercept = 1, linetype = 'dotted') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(y = paste(strsplit(var,'\\.')[[1]][1]),
         x = paste0(strsplit(rets,'\\.')[[1]][1],'s')) +
    ggplot2::ggtitle(paste(strsplit(var,'\\.')[[1]][1] ,'vs', paste0(strsplit(rets,'\\.')[[1]][1],'s') ) )
  return(p)

}


#' Plot pnl in month-year matrix
#'
#' @param this modelStrategy
#' @param from character, name of backtest
#' @param compounded logical, compounded returns to use or not
#' @param ... params
#'
#' @export
#' @rdname plotCalendar
#' @method plotCalendar Strategy
plotCalendar.Strategy <- function(this, compounded = FALSE, ...){
  M <- apply.monthly(getPnL(this), FUN = function(x){
    if(compounded){
      (tail(x, 1)[[1]] - head(x, 1)[[1]]) / head(x, 1)[[1]] * 100
    }else{
      (tail(x, 1)[[1]] - head(x, 1)[[1]]) / getMoney(this) * 100
    }
  }) %>%
    set_colnames('rets') %>%
    cbind(year = lubridate::year(index(.))) %>%
    cbind(month  = lubridate::month(index(.))) %>%
    coredata %>%
    data.frame %>%
    {
      reshape2::dcast(.,month ~ year,value.var = 'rets')[,-1]
    } %>%
    set_rownames(month.name) %>%
    as.matrix %>%
    {
      .[is.na(.)] <- 0
      .
    }
  corrplot::corrplot(M, method="color",
                     col=colorRampPalette( c("red", "white", "green"), space="rgb")(200),
                     addCoef.col = "black",
                     tl.col="black",
                     tl.srt=45, #Text label color and rotation
                     insig = "blank",
                     cl.pos = 'n',
                     is.corr = FALSE
  )
}



#' @export
#' @rdname plotCalendar
#' @method plotCalendar xts
plotCalendar.xts <- function(this, ...){
  M <- apply.monthly(this, FUN = function(x){
    (tail(x, 1)[[1]] - head(x, 1)[[1]]) / head(x, 1)[[1]] * 100
  }) %>%
    set_colnames('rets') %>%
    cbind(year = lubridate::year(index(.))) %>%
    cbind(month  = lubridate::month(index(.))) %>%
    coredata %>%
    data.frame %>%
    {
      reshape2::dcast(.,month ~ year,value.var = 'rets')[,-1]
    } %>%
    set_rownames(month.name) %>%
    as.matrix %>%
    {
      .[is.na(.)] <- 0
      .
    }
  corrplot::corrplot(M, method="color",
                     col=colorRampPalette( c("red", "white", "green"), space="rgb")(200),
                     addCoef.col = "black",
                     tl.col="black",
                     tl.srt=45, #Text label color and rotation
                     insig = "blank",
                     cl.pos = 'n',
                     is.corr = FALSE
  )
}




#' Plot Capital of strategy
#'
#'
#' @param this modelStrategy
#' @param ... params
#' @export
#' @rdname plotCapital
plotCapital <- function(this,
                        ...){
  UseMethod('plotCapital', this)
}


#' Get Capital of strategy
#'
#' @param ... params for plotCapital
#' @export
#' @rdname getCapital
getCapital <- function(...){
  args <- list(...)
  args[['return_type']] <- 'data'
  x <- do.call('plotCapital', args = args)
  x[is.na(x)] <- 0
  return(x)
}



#' @param start_date Date / character, example: start_date='2008-01-01'
#'
#' @param interactive_plot logical, if it is TRUE then plot will be intercative
#' @param leg numeric / character, numeric is responsible for capital by legs, character can be "all" then capital will be summed or it can be "sep" then
#' capital will be plotted for each leg
#' @param end_date Date / character, example: end_date='2018-01-01'
#' @param return_type character, enter 'plot' for graphical representation or 'data' for xts series.
#'
#' @export
#' @rdname plotCapital
#' @method plotCapital Strategy
plotCapital.Strategy <- function(this,
                                 interactive_plot = TRUE,
                                 start_date = NULL,
                                 end_date = NULL,
                                 leg = 'all',
                                 return_type = 'plot',
                                 ...){
  e <- this$backtest
  dates <- getDateByIndex(this)
  if (!is.null(start_date)){
    range_start <- max(e$activeField['start'],  sum(dates < start_date) + 1)
  }
  else{
    range_start <- e$activeField['start']
  }
  if(!is.null(end_date)){
    range_end <- min(e$activeField['end'], sum(dates < end_date))
  }
  else{
    range_end <- e$activeField['end']
  }
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  legs <- leg
  leg <- legs[1]
  if(leg == 'all'){
    x <- e$results$money_in_pos
    x[x == 0] <- NA
  }else if(leg == 'sep'){
    x <- e$results$money_in_pos_leg
    x[x == 0] <- NA
  }else if(is.numeric(leg)){
    x <- e$results$money_in_pos_leg[,legs]
    x[x == 0] <- NA
  }

  df <- cbind(
    data.frame(date=dates),
    data.frame(Money = x)
  )[range,]
  if(leg == 'sep'){
    colnames(df) <- c('date', colnames(getModelD(this)$data_raw))
  }else if(is.numeric(leg)){
    colnames(df) <- c('date', colnames(getModelD(this)$data_raw)[legs])
  }
  if(return_type == 'plot'){
    newdf <- reshape2::melt(df, 'date')
    p <- ggplot2::ggplot(newdf,aes_string(x="date", y="value", color = "variable") ) +
      ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("Money in position") #+ theme(legend.position = "none")
    if(leg != 'sep'){
      p + ggplot2::scale_color_manual(
        values = c(
          PnL = 'darkblue'
        )) + ggplot2::theme(legend.position="none")
    }
    if(interactive_plot){
      return(plotly::ggplotly(p))
    }
    p
  }else if(return_type == 'data'){
    return(xts(df[,-1], df$date))
  }

}


#' Draws 5-D graph with axis x,y,size,color,symbol, which are contained in data.frame
#'
#' @param df data.frame
#' @param x character/expression type, axis x, default NULL
#' @param y character/expression type, axis y, default NULL
#' @param size character/expression type, axis size, default NULL
#' @param color character/expression type, axis color, default NULL
#' @param symbol character/expression type, axis symbol, default NULL
#' @param size_scale numeric type, point size, default 20
#' @return plot_ly object
#' @export
#' @rdname plotTable
plotTable <- function(df, x=NULL, y=NULL ,size=NULL, color=NULL, symbol=NULL, size_scale = 20){
  x <- rlang::enexpr(x)
  y <- rlang::enexpr(y)
  size <- rlang::enexpr(size)
  color <- rlang::enexpr(color)
  symbol <- rlang::enexpr(symbol)
  if(is.character(x)){
    x <- as.symbol(x)
  }
  if(is.character(y)){
    y <- as.symbol(y)
  }
  if(is.character(color)){
    color <- as.symbol(color)
  }
  if(is.character(size)){
    size <- as.symbol(size)
  }
  if(is.character(symbol)){
    symbol <- as.symbol(symbol)
  }
  if(is.null(size)){
    size <- NULL
    sizeref <- NULL
  }else{
    sizeref <- call('~', rlang::expr(2.0 * max(!!size) / (!!size_scale)**2 ))
  }
  if(is.null(symbol)){
    symbol <- NULL
  }else{
    if (length(unique(df[[as.character(symbol)]])) > 6){
      stop("The shape palette can deal with a maximum of 6 discrete values because more than 6 becomes difficult to discriminate")
    }
    symbol <- call("~",call("factor",rlang::expr(!!symbol)))
  }
  if(is.null(color)){
    color <- NULL
  }else{
    color <- call("~",rlang::expr(!!color))
  }

  q <- rlang::call2('paste', "<br>",!!!sapply(names(df), function(x){c(paste("<br>",x,":"),as.symbol(x))}))
  expr <- rlang::call2(quote(plotly::plot_ly), data = quote(df), x = call("~", x),
                       y = call("~", y),
                       color = color, hoverinfo = "text",
                       text = call("~", q), symbol = symbol, type = "scatter",
                       mode = "markers",
                       marker = rlang::call2("list",
                                             size = call("~", size),
                                             opacity = 0.5,
                                             sizemin = 2,
                                             sizemode = 'area',
                                             sizeref = sizeref))
  eval(expr)
}



#' Draws 5-D graph with axis x,y,size,color,symbol, which are contained in data.frame
#'
#' @param df data.frame
#' @param x character/expression type, axis x, default "sharpe.ann"
#' @param y character/expression type, axis y, default "sortino.ann"
#' @param size character/expression type, axis size, default NULL
#' @param color character/expression type, axis color, default NULL
#' @param symbol character/expression type, axis symbol, default NULL
#' @param size_scale numeric type, point size, default 20
#'
#' @return plot_ly object
#' @export
#' @rdname plotParamset
#' @method plotParamset data.frame
plotParamset.data.frame <- function(df, x = NULL, y = NULL, size = NULL,
                                    color = NULL, symbol = NULL, size_scale = 20){
  cl <- rlang::call2('plotTable', !!!rlang::enexprs(df=df, x=x, y=y, size=size, color=color, symbol=symbol,
                                                    size_scale=size_scale))
  eval(cl)
}

#' Plot backtests results in 5-D graph
#'
#' @param this Strategy
#' @param x character/expression type, axis x, default NULL
#' @param y character/expression type, axis y, default NULL
#' @param size character/expression type, axis size, default NULL
#' @param color character/expression type, axis color, default NULL
#' @param symbol character/expression type, axis symbol, default NULL
#' @param size_scale numeric type, point size, default 20
#'
#' @export
#' @rdname plotParamset
#' @method plotParamset Strategy
plotParamset.Strategy <- function(this, x = NULL, y = NULL, size = NULL,
                                  color = NULL, symbol = NULL, size_scale = 20){
  cl <- rlang::call2('plotParamset.data.frame', df=quote(getBacktestResults(this)),
                     !!!rlang::enexprs(x=x, y=y, size=size, color=color, symbol=symbol, size_scale=size_scale))
  eval(cl)
}



