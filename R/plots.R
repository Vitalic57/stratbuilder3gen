add_vline = function(p, x, ...) {
  if(length(x) > 1){
    for(i in seq_along(x)){
      p <- p %>% add_vline(x[i], ...)
    }
    return(p)
  }
  if(!is.null(p$x$layoutAttrs)){
    index <- unname(which(sapply(p$x$layoutAttrs, function(x)
      !is.null(x$shapes))))
  } else {
    index <- integer()
  }

  l_shape = list(
    type = "line",
    y0 = 0, y1 = 1, yref = "paper", # i.e. y as a proportion of visible region
    x0 = x, x1 = x,
    line = list(
      ...
    ),
    layer = "below"
  )

  if(length(index) > 0){
    shapes <- p$x$layoutAttrs[[index]]$shapes
    shapes[[length(shapes) + 1]] <- l_shape
    p$x$layoutAttrs[[index]]$shapes <- shapes
  } else {
    p <- plotly::layout(
      p = p,
      shapes = list(l_shape)
    )
  }
  p
}


#' Plot change of profit and loss through backtest period
#'
#' @param this Strategy
#' @param leg numeric/character, number or numbers of legs, if it is equal to 'all' or 'sum', then all pnl among all legs
#' will be summed, if it is equal to 'sep', then pnl among legs will be plotted
#' @param graph_type character, ggplot2 / xts / plotly
#' @param each_year logical, if TRUE, then each graph will start with 0 each year
#' @param adjust logical, if TRUE, then values will be divided by getMoney(this)
#' @param comOn bool, if true then commission will be included in the 'trades' graph
#' @param return_type character, plot or data
#' @param cutoff logical, if true then vertical line will plotted on graph where model was created
#'
#' @export
#' @rdname plotPnL
#' @method plotPnL Strategy
plotPnL.Strategy <- function(this,
                             comOn = TRUE,
                             leg = 'all',
                             graph_type = 'ggplot2',
                             each_year = FALSE,
                             adjust = FALSE,
                             return_type = 'plot',
                             cutoff = FALSE,
                             start,
                             end){
   e <- this$backtest
   legs <- leg
   leg <- legs[1]
   dates <- getDateByIndex(this)
   range_start <- get_backtest_start_index(this, start)
   range_end <- get_backtest_end_index(this, end)
   if(range_start > range_end){
     return(NULL)
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
       data.frame(PnL = init_money + exchange_to_counter(this$data, e$results$unrealized_money[,leg,drop=FALSE], 1:this$data$nrow) +
                    e$results$realized_money[,leg] +
                    cumsum((1 - comOn) * e$results$commissions_table[, leg]))
     )[range,]
   }else if(leg %in% c('sep', 'separate')){
     leg <- 'sep'
     df <- cbind(
       data.frame(date=dates),
       data.frame(init_money + exchange_to_counter(this$data, e$results$unrealized_money, 1:this$data$nrow) +
                    e$results$realized_money +
                    apply( (1 - comOn) * e$results$commissions_table, 2, cumsum)) %>%
         set_colnames(getData(this)$colnames)
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
     if(graph_type %in% c('ggplot2', 'plotly')){
       newdf <- reshape2::melt(df, 'date')

       if(graph_type == 'plotly'){
         if(ncol(df) == 2){
           p <- plotly::plot_ly(newdf, x = ~date, y = ~value, mode ='lines', type = 'scatter', line = list(color = "darkblue", width = 1.5))
         }else{
           p <- plotly::plot_ly(newdf, x = ~date, y = ~value, mode ='lines', type = 'scatter', color = ~variable, colors='Set1', line = list(width = 1.5))
         }
         p <- p %>%
           plotly::layout(title = list(text="PnL by date", x=0.1))
         if(each_year){
           p <- p %>% add_vline(x=as.Date(last_dates, origin = '1970-01-01'), dash='dash', color="red")
         }
         if(cutoff && 'created' %in% names(this)){
           p <- p %>% add_vline(x=this$created, dash='dash', color="green")
         }
         p
       }else{
         p <- ggplot2::ggplot(newdf, ggplot2::aes(x=date, y=value, color = variable) ) +
           ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("PnL money by date")
         if(each_year){
           p <- p + ggplot2::geom_vline(xintercept=last_dates, linetype=4, colour="red")
         }
         if(cutoff && 'created' %in% names(this)){
           p <- p + ggplot2::geom_vline(xintercept=as.numeric(this$created), linetype=4, colour="green")
         }
         if(leg[1] != 'sep'){
           p <- p + ggplot2::scale_color_manual(
             values = c(
               PnL = 'darkblue'
             )) + ggplot2::theme(legend.position="none")
         }
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
}

#' Plot drawdowns
#'
#' @param this Strategy
#' @param from character, name of backtest
#' @param start Numeric scalar / Date
#' @param end Numeric scalar / Date
#' @param return_type character, plot or data
#' @param graph_type character, ggplot2 / xts / plotly
#' @param ... params
#'
#' @return ggplot/xts
#' @export
#' @rdname plotDrawdowns
#' @method plotDrawdowns Strategy
plotDrawdowns.Strategy <- function(this,
                                   start,
                                   end,
                                   return_type = 'plot',
                                   graph_type = 'ggplot2',
                                   ...){
  e <- this$backtest
  dates <- getDateByIndex(this)
  range_start <- get_backtest_start_index(this, start)
  range_end <- get_backtest_end_index(this, end)
  # range_start <- e$activeField['start']
  # range_end <- e$activeField['end']
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  df <- cbind(
    data.frame(date=dates[range]),
    data.frame(PnL = calcStat(this, acceptable_stats$pnl, range_start, range_end) %>% {. - cummax(.)})
  )
  if(return_type == 'plot'){
    if(graph_type %in% c('ggplot2', 'plotly')){
      newdf <- reshape2::melt(df, 'date')

      if(graph_type == 'plotly'){
        return(plotly::plot_ly(newdf, x = ~date, y = ~value, mode ='lines', type = 'scatter',  line = list(color = "red", width = 1.5)) %>%
                 plotly::layout(title = list(text="Drawdowns by date", x=0.1)))
      }else{
        ggplot2::ggplot(newdf, ggplot2::aes(x=date, y=value, color = variable) ) +
          ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::theme(legend.position="none") +
          ggplot2::scale_color_manual(
            values = c(
              PnL = 'red'
            ))+
          ggplot2::ggtitle("Drawdowns by date")
      }
    }else{
      plot(xts(df[,'PnL'], df[,'date']), format.labels = '%Y-%m-%d', main = 'PnL', ylab = 'money')
    }
  }else if(return_type == 'data'){
    return(xts(df[,'PnL'], df[,'date']))
  }
}

#' Plot trade PnL vs MAE/MFE in trade
#' 
#' Red triangles is losing trades, green triangles is winning trades. Grey dashed line is y = x line.
#' On y-axis MAE/MFE is displayed, on x-axis PnL of trade is displayed.
#'
#' @param this Strategy
#' @param type character, MAE or MFE
#'
#' @return ggplot
#' @export
#' @rdname plotTrades
#' @method plotTrades Strategy
plotTrades.Strategy <- function(this, type = 'MAE'){
  e <- this$backtest
  report <- getTrades(this) %>%
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
  p <- ggplot2::ggplot(df, ggplot2::aes(abs(rets),var, group = rets < 0 )) +
    ggplot2::geom_point(ggplot2::aes(col = rets < 0, shape = rets  < 0) , size = 3) +
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
#' @param this Strategy
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
#' @param this Strategy
#' @param ... params
#' @export
#' @rdname plotPosition
plotPosition <- function(this,
                        ...){
  UseMethod('plotPosition', this)
}


#' Get amount of money in position
#'
#' @param ... params for plotPosition
#' @export
#' @rdname plotPosition
getPosition <- function(...){
  args <- list(...)
  args[['return_type']] <- 'data'
  x <- do.call('plotPosition', args = args)
  x[is.na(x)] <- 0
  return(x)
}



#' @param start Numeric scalar / Date / character, example: start_date='2008-01-01'
#'
#' @param leg numeric / character, numeric is responsible for capital by legs, character can be "all" then capital will be summed or it can be "sep" then
#' capital will be plotted for each leg
#' @param end Numeric scalar / Date / character, example: end_date='2018-01-01'
#' @param return_type character, enter 'plot' for graphical representation or 'data' for xts series.
#' @param graph_type character, ggplot2 or plotly
#'
#' @export
#' @rdname plotPosition
#' @method plotPosition Strategy
plotPosition.Strategy <- function(this,
                                 start,
                                 end,
                                 leg = 'all',
                                 return_type = 'plot',
                                 graph_type = 'ggplot2',
                                 ...){
  e <- this$backtest
  dates <- getDateByIndex(this)
  range_start <- get_backtest_start_index(this, start)
  range_end <- get_backtest_end_index(this, end)
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  legs <- leg
  leg <- legs[1]
  if(leg == 'all'){
    x <- calcStat(this, acceptable_stats$money_in_pos_, range_start, range_end)
    x[x == 0] <- NA
  }else if(leg == 'sep'){
    x <- abs(e$results$money_in_pos_leg)
    x[x == 0] <- NA
  }else if(is.numeric(leg)){
    x <- abs(e$results$money_in_pos_leg[,legs])
    x[x == 0] <- NA
  }

  df <- cbind(
    data.frame(date=dates),
    data.frame(Money = x)
  )[range,]
  if(leg == 'sep'){
    colnames(df) <- c('date', getData(this)$colnames)
  }else if(is.numeric(leg)){
    colnames(df) <- c('date', getData(this)$colnames[legs])
  }
  if(return_type == 'plot'){
    newdf <- reshape2::melt(df, 'date')
    if(graph_type == 'plotly'){
      return(plotly::plot_ly(newdf, x = ~date, y = ~value,
                             mode ='lines', type = 'scatter', color = ~variable, colors='Set1', line = list(width = 1.5)) %>%
               plotly::layout(title = list(text="Money in position", x=0.1)))
    }else{
      p <- ggplot2::ggplot(newdf, ggplot2::aes_string(x="date", y="value", color = "variable") ) +
        ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("Money in position")
      if(leg != 'sep'){
        p <- p + ggplot2::scale_color_manual(
          values = c(
            Money = 'red'
          )) + ggplot2::theme(legend.position="none")
      }
      return(p)
    }
  }else if(return_type == 'data'){
    return(xts(df[,-1], df$date))
  }

}


#' Plot Net position of strategy
#'
#'
#' @param this Strategy
#' @param ... params
#' @export
#' @rdname plotNetPosition
plotNetPosition <- function(this,
                        ...){
  UseMethod('plotNetPosition', this)
}


#' Get Capital of strategy
#'
#' @param ... params for plotNetPosition
#' @export
#' @rdname plotNetPosition
getNetPosition <- function(...){
  args <- list(...)
  args[['return_type']] <- 'data'
  x <- do.call('plotNetPosition', args = args)
  x[is.na(x)] <- 0
  return(x)
}



#' @param start Numeric scalar / Date / character, example: start_date='2008-01-01'
#'
#' @param leg numeric / character, numeric is responsible for capital by legs, character can be "all" then capital will be summed or it can be "sep" then
#' capital will be plotted for each leg
#' @param end Numeric scalar / Date / character, example: end_date='2018-01-01'
#' @param return_type character, enter 'plot' for graphical representation or 'data' for xts series.
#' @param graph_type character, ggplot2 or plotly
#'
#' @export
#' @rdname plotNetPosition
#' @method plotNetPosition Strategy
plotNetPosition.Strategy <- function(this,
                                 start,
                                 end,
                                 leg = 'all',
                                 return_type = 'plot',
                                 graph_type = 'ggplot2',
                                 ...){
  e <- this$backtest
  dates <- getDateByIndex(this)
  range_start <- get_backtest_start_index(this, start)
  range_end <- get_backtest_end_index(this, end)
  if(range_start > range_end){
    stop("start > end")
  }
  range <- range_start:range_end
  legs <- leg
  leg <- legs[1]
  if(leg == 'all'){
    x <- calcStat(this, acceptable_stats$net_pos_, range_start, range_end)
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
    colnames(df) <- c('date', getData(this)$colnames)
  }else if(is.numeric(leg)){
    colnames(df) <- c('date', getData(this)$colnames[legs])
  }
  if(return_type == 'plot'){
    newdf <- reshape2::melt(df, 'date')
    if(graph_type == 'plotly'){
      return(plotly::plot_ly(newdf, x = ~date, y = ~value,
                             mode ='lines', type = 'scatter', color = ~variable, colors='Set1', line = list(width = 1.5)) %>%
               plotly::layout(title = list(text="Net position", x=0.1)))
    }else{
      p <- ggplot2::ggplot(newdf, ggplot2::aes_string(x="date", y="value", color = "variable") ) +
        ggplot2::geom_line() + ggplot2::theme_bw() + ggplot2::ggtitle("Net position")
      if(leg != 'sep'){
        p <- p + ggplot2::scale_color_manual(
          values = c(
            Money = 'red'
          )) + ggplot2::theme(legend.position="none")
      }
      return(p)
    }
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
  cl <- rlang::call2('plotParamset.data.frame', df=rlang::expr(getBacktestResults(!!this)),
                     !!!rlang::enexprs(x=x, y=y, size=size, color=color, symbol=symbol, size_scale=size_scale))
  eval(cl)
}



