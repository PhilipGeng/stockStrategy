currentScore <- function(high, low, currentPrice) {
    if (high == low) return (50)
    return (100*(currentPrice - low) / (high - low))
}
periodReturn <- function(NewPrice, oldPrice) {
    return (100*(NewPrice/oldPrice - 1))
}


set.seed(100)   # randomize buying order
strategy <- function(open, high, low, close, amt, buyable, sellable, initial.cash, transaction){
    
    require(TTR)
    
    #####  Set parameters  #####
    startDay <- 30      # hold-out period for observation
    stopRatio <- 0.92   # stopLoss bound
    betSize <- 5.5      # amount for each buy
    mlBuyThreshold <- 8 # in %
    REG_FREQ <- 10      # frequency in days of running regression
    
    #####  Cast data into matrix for easy computations  #########################
    position.matrix <- close*0                        # final output as dataframe
    open <- data.matrix(open, rownames=NA)
    high <- data.matrix(high, rownames=NA)
    low <- data.matrix(low, rownames=NA)
    close <- data.matrix(close, rownames=NA)
    amt <- data.matrix(amt, rownames=NA)
    buyable <- data.matrix(buyable, rownames=NA)
    sellable <- data.matrix(sellable, rownames=NA)
    
    #####  portfolio-, strategy-related info  #####
    nDay <- nrow(close)
    nStock <- ncol(close)
    pm <- matrix(0, nDay, nStock)        # position matrix in MATRIX class; convert to df before output
    cash <- rep(0, nDay)                 # daily cash balance
    buyReq <- rep(0, nStock)             # buy request holder
    sellReq <- rep(0, nStock)            # sell request holder
    #stopPrice <- rep(0, nStock)          # stop loss order on price
    #entryPrice <- rep(0, nStock)         # another stop loss order on price
    entryMoney <- rep(0, nStock)         # another
    tradable <- matrix(TRUE, nDay, nStock)    # we stop buying that security for 5 days if we just sell them 
    
    #####  indicators-related info  #####
    rsi <- apply(close, 2, RSI, n=20)           # indicators for each stock on each day
    lowestLow <- matrix(0, nDay, nStock)        # names are self-explantory
    highestHigh <- matrix(0, nDay, nStock)
    lowestLowDate <- matrix(1, nDay, nStock)
    highestHighDate <- matrix(1, nDay, nStock)
    daySinceHighest <- matrix(0, nDay, nStock)
    daySinceLowest <- matrix(0, nDay, nStock)
    recentScore <- matrix(0, nDay, nStock)      # main indicator:  = 1-william%R 
    
    #############  compute all indicators information  ######################
    #############  NO FUTURE INFO IS USED EXCEPT UP TO THAT DAY  ############
    for (j in 1:nStock) {
        recentScore[,j] <- 1 - WPR(cbind(high[,j],low[,j],close[,j]), n=20)
    }
    
    highestHigh[1,] <- high[1,]
    lowestLow[1,] <- low[1,]
    for (i in 2:nDay) {
        highestHigh[i,] <- highestHigh[i-1,]
        highestHighDate[i,] <- highestHighDate[i-1,]
        lowestLow[i,] <- lowestLow[i-1,]
        lowestLowDate[i,] <- lowestLowDate[i-1,]
        
        for (j in 1:nStock) {
            if (highestHigh[i,j] <= high[i,j]) {
                highestHigh[i,j] <- high[i,j]
                highestHighDate[i,j] <- i
            }
            if (lowestLow[i,j] >= low[i,j]) {
                lowestLow[i,j] <- low[i,j]
                lowestLowDate[i,j] <- i
            }
        }
    }
    for (i in 20:nDay) {
        daySinceHighest[i,] <- (i-10 - highestHighDate[i-10,])/(i-10)
        daySinceLowest[i,] <- (i-10 - lowestLowDate[i-10,])/(i-10)
    }
    
    #####  Statistical Learning Part -- Linear Regression  #####
    nmldata <- floor(nDay/30)                # nmldata = how many training we can do
    mldata <- matrix(0, nmldata*100, 6)      # this holds the training data 
    mldata <- data.frame(mldata)
    colnames(mldata) <- c('x1', 'x2', 'x3', 'x4', 'y1', 'y2')
    mlcounter <- 0
    tempy1 <- rep(0, nStock)
    tempy2 <- rep(0, nStock)
    mlTest <- matrix(0, nStock, 4)           # this holds the daily data for prediction
    mlTest <- data.frame(mlTest)
    colnames(mlTest) <- c('x1', 'x2', 'x3', 'x4')
    
    #############  not trading in first (startDay - 1) days  ######################
    #############  so cash level   #########################

    cash[1:(startDay-1)] <- initial.cash
    
    ############# initialize complete; start trading~  ######################
    
    for (i in startDay:nDay) {
        
        ################  first update positions, then check signals  #########
        #####  load positions from previous day  #####
        pm[i,] <- pm[i-1,]
        cashAvail <- cash[i-1]
        
        #####  execute buy request from previous day  #####
        if (i < 41) {                                # regression starts on day40
            amtavg <- colMeans(amt[1:(i-1),])        # if no regression, 
            amtavg <- sample(order(amtavg))          # buy in random order 
        } else {
            amtavg <- order(-mlPredSell)             # if regression prediction available, 
        }                                            # buy the stocks with least predicted loss first
        
        for (j in 1:nStock) {
            sidx <- amtavg[j]               # index to buy
            
            if (buyReq[sidx] == 1) {
                if (buyable[i,sidx] == 1) {
                    if (cashAvail > 0) {
                        moneyInvest <- ifelse(cashAvail < betSize, cashAvail*0.5, betSize)
                        sharesBought <-  floor(moneyInvest*100/open[i,sidx])/100  
                        pm[i,sidx] <- pm[i,sidx] + sharesBought
                        entryMoney[sidx] <- entryMoney[sidx] + sharesBought*open[i,sidx]*(1+transaction)
                        cashAvail <- cashAvail - entryMoney[sidx]
                        #stopPrice[sidx] <- open[i,sidx]
                        #entryPrice[sidx] <- open[i,sidx]
                    } 
                } 
                buyReq[sidx] <- 0
            }
        }
        
        #####  execute sell request from previous day  #####
        for (j in 1:nStock) {
            if (sellReq[j] == 1) {
                if (sellable[i,j] == 1) {
                    cashAvail <- cashAvail + pm[i-1,j]*close[i,j]*(1-transaction)
                    pm[i,j] <- 0
                    sellReq[j] <- 0
                } 
            } 
        }
        
        cash[i] <- cashAvail
        ################  finish updating positions  ########################
        #####################################################################
        ################  begin checking buy/sell signals  ##################
        
        if (i >= 40) {
            if (i %% REG_FREQ == 0) {        # run regression training every REG_FREQ days
                                             # for detail explanation, please refer to report section 3.2
                recentScoreAvg5 <- colMeans(recentScore[(i-14):(i-10),])
                rsiAvg5 <- colMeans(rsi[(i-14):(i-10),])/100
                mlIdx <- (mlcounter*nStock+1):(mlcounter*nStock+nStock)
                mldata[mlIdx,1:4] <- cbind(recentScoreAvg5, rsiAvg5, daySinceHighest[i,], daySinceLowest[i,])
                for (j in 1:nStock) {
                   tempy1[j]  <- periodReturn(max(high[(i-9):i,j]), open[i-9,j])
                   tempy2[j] <- periodReturn(min(low[(i-9):i,j]), open[i-9,j])
                }
                mldata[mlIdx,5] <- tempy1
                mldata[mlIdx,6] <- tempy2 
                
                reg.buy <- lm(y1 ~ x1+x2+x3+x4, data=mldata[mlIdx,])
                reg.sell <- lm(y2 ~ x1+x2+x3+x4, data=mldata[mlIdx,])
                
                mlcounter <- mlcounter + 1
            }
            
            #####  run regression predictions everyday after day40  #####
            mlTest[,1:4] <- cbind(recentScore[i,], rsi[i,]/100, daySinceHighest[i,], daySinceLowest[i,])
            mlPredBuy <- predict(reg.buy, mlTest)        # predicted highest return on future 10 days
            mlPredSell <- predict(reg.sell, mlTest)      # predicted lowest return on future 10 days
        }
        
        #####  buy signals  #####
        for (j in 1:nStock) {
            if (tradable[i,j] && pm[i,j] < 2) {
                buySignal <- rep(FALSE, 3)
                
                if (recentScore[i,j] > 0.85) {
                    buySignal[1] <- TRUE
                    buySignal[2] <- TRUE
                }
                
                if (buySignal[2] && (i - highestHighDate[i,j] > 100)) {
                    allScore <- currentScore(highestHigh[i,j], lowestLow[i,j], close[i,j])
                    if (allScore < 70) {
                        buySignal[2] <- FALSE
                    }
                }  
                
                if (i < 40) {      # buy signals from regression after day40
                    buySignal[3] <- TRUE
                } else {
                    if (mlPredBuy[j] > mlBuyThreshold) {
                        buySignal[3] <- TRUE
                    }
                }

                if (sum(buySignal) >= 2) {
                    buyReq[j] <- 1
                }
            }
        }
        
        #####  sell signals  #####
        for (j in 1:nStock) {
            if (pm[i,j] > 0) {
                sellSignal <- rep(FALSE, 4)
                stopLossPrice <- min(close[(i-5):(i-1),j])
                if (close[i,j] < stopLossPrice*stopRatio) {
                    sellSignal[1] <- TRUE
                }

                if (sum(sellSignal) >= 1) {
                    sellReq[j] <- 1
                    tradable[(i+1):(i+5),j] <- FALSE      # prevent buying this asset just after selling
                }
                
            }
        }
        
    }
    position.matrix[,1:nStock] <- pm
    return(position.matrix)
}


####################################
position.matrix <- strategy(OPEN, HIGH, LOW, CLOSE, AMT, BUYABLE, SELLABLE, initial.cash, transaction)
performance.summary(OPEN, HIGH, LOW, CLOSE, AMT, BUYABLE, SELLABLE, initial.cash, transaction, position.matrix)



