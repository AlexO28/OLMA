library(RODBC)

GetDealsAndOrdersFromBase <- function(datestart, dateend, port1, port2) {
  #deals only for dollar
  ch <- odbcConnect("mydata", "reader", "123")
  constr <- "select [Date], [Security], Price, Volume, Side, Commission, Comment from dbo.DBTrades (nolock) where (Portfolio = '@p1'  or Portfolio = '@p2')
  and ([Date] >= '@d1') and  ([Date] <= '@d2 23:59:59')"

  constr <- gsub("@d1", datestart, constr)
  constr <- gsub("@d2", dateend, constr)
  constr <- gsub("@p1", port1,  constr)
  constr <- gsub("@p2", port2,  constr)
  print(constr)
  tabdol <- sqlQuery(ch, constr)
  #orders only for futures
  constr <- "select [Date], [Security], Price, Volume, Side, Comment from dbo.DBOrders (nolock) where (Portfolio = '@p1'  or Portfolio = '@p2')
  and ([Date] >= '@d1') and  ([Date] <= '@d2 23:59:59')"
  constr <- gsub("@d1", datestart, constr)
  constr <- gsub("@d2", dateend, constr)
  constr <- gsub("@p1", port1,  constr)
  constr <- gsub("@p2", port2,  constr)
  print(constr)

  tabfut <- sqlQuery(ch, constr)
  odbcClose(ch)
  print(str(tabfut))

  tabdol  <- as.data.table(tabdol)
  tabdol[, id := 1:nrow(tabdol)]
  tabfut <- as.data.table(tabfut)
  tabdol[, Time := fastPOSIXct(tabdol$Date)]
  tabdol[, Date := as.Date(tabdol$Date)]
  tabfut[, Time := fastPOSIXct(tabfut$Date)]
  tabfut[, Date := as.Date(tabfut$Date)]
  tabdol <- tabdol[Security %in% c("USD000UTSTOM@CETS", "USD000000TOD@CETS"), ]
  tabdol[, Direction := Side]
  tabdoltod <- tabdol[Security == "USD000000TOD@CETS", ]
  tabdoltom <- tabdol[Security == "USD000UTSTOM@CETS", ]
  if (nrow(tabdoltod) > 0) {
    res <- BrandNewFullMatch(tabdoltod, tabdoltom, fullmode = TRUE)
    tabdol <- res$tab2
    names(tabdol) <- capitalize(names(tabdol))
  }
  tabfut <- tabfut[Security == "SIU6@FORTS", ]
  #setkey(tabdol, "Time")
  #setkey(tabfut, "Time")
  #tabdol <- tabdol[order(tabdol$Time), ]
  #tabfut <- tabfut[order(tabfut$Time), ]
  tabdol$TimeNum <- as.numeric(tabdol$Time)# - as.numeric(tabdol$Time[1])
  tabfut$TimeNum <- as.numeric(tabfut$Time)# - as.numeric(tabfut$Time[1])
  setkey(tabdol, "Time")
  setkey(tabfut, "Time")
  tabdol[Side == "Sell", Volume := - Volume]
  tabfut[Side == "Sell", Volume := - Volume]
  tabdol[, id := 1:nrow(tabdol)]
  tabfut[, id := 1:nrow(tabfut)]
list(deals = tabdol, orders = tabfut)
}
ProcessHedgeDeals <- function(dat) {
  deals <- HedgeComparison2(dat, lowlen = -0.4, uplen = 0.4)
  print(summary(deals))
  print(deals[is.na(hedgeid), ])
  mtab <- GetHedgeDiff(dat, deals)
  print(mtab$val)
  tab <- mtab$tab
data.table(Price = tab$price, Volume = tab$volume, Side = tab$side,
          Direction = tab$side, Time = tab$time,
          TimeNum = as.numeric(tab$time),
          InitPrice = as.numeric(substr(tab$comment, 1, 5)),
          Comment = substr(tab$comment, 6, 6))
}
GetProcessedDeals <- function() {
  deals <- fread("D:\\dealsforhedgetest.csv")
  deals <- deals[Security %in% c("USD000UTSTOM@CETS", "SIU6@FORTS"), ]
  futs <- deals$Security == "SIU6@FORTS"
  dols <- deals$Security == "USD000UTSTOM@CETS"
  dols <- c(FALSE, dols[1:(length(dols)-1)])
  print(head(futs))
  print(head(dols))
  deals <- deals[futs & dols, ]
  deals[, InitPrice := as.numeric(substr(Comment, 1, 5))]
}
CheckHedgeAlgo <- function(deals, initslippage, T) {
  deals <- as.data.table(as.data.frame(deals))
  dates <- unique(deals$Date)
  res <- 0
  for (adate in dates) {
    dealsred <- deals[Date == adate,]
    res <- res + CheckHedgeAlgoDay(dealsred, initslippage, T, adate)
    print(paste0("res", res))
  }
res/nrow(deals)
}
CheckHedgeTicksAlgo <- function(deals, initslippage, T, Tlen, cval, tab) {
  deals <- as.data.table(as.data.frame(deals))
  slipvec <- numeric(nrow(deals))
  for (j in 1:nrow(deals)) {
    print(j)
    deal <- deals[j, ]
    tabred <- tab[TimeNum >= deal$TimeNum[1] & TimeNum <= deal$TimeNum[1] + Tlen
                  , ]

    slipvec[j] <- CheckHedgeTicksAlgoDeal(deal, tabred, initslippage, T, cval)
  }
  mdeals <- deals[!is.na(slipvec), ]
c(sum(abs(mdeals$Volume)*slipvec[!is.na(slipvec)])/sum(abs(mdeals$Volume)), nrow(mdeals))
#  c(mean(slipvec[!is.na(slipvec)]), nrow(mdeals))
}
CheckHedgeTicksAlgoDeal <- function(deal, tabred, initslippage, T, cval) {
  adir <- deal$Direction[1]
  initprice <- deal$InitPrice[1]
  print(adir)
  print(initslippage)
  print(initprice)
  myorder <- initprice + ifelse(adir == "Buy", 1, -1)*initslippage
  inittime <- deal$TimeNum[1]
  onchange <- TRUE
  if (nrow(tabred) == 0) {return(NA)}
  for (i in 1:nrow(tabred)) {
    if (tabred$TimeNum[i] - inittime >= T) {
       #order change case
       myorder <- myorder + ifelse(adir == "Buy", 1, -1)*initslippage*
         trunc((tabred$TimeNum[i] - inittime)/T)
       inittime <- tabred$TimeNum[i]
       onchange <- TRUE
    }
      #stoploss case
      if (adir == "Sell") {
        if (initprice - tabred$Price[i] >= cval) {
          print("stoploss sell")
          return(initprice - tabred$Price[i])
        }
      } else if (adir == "Buy") {
        if (tabred$Price[i] - initprice >= cval) {
          print("stoploss buy")
          return(tabred$Price[i] - initprice)
        }
      }
#    if (tabred$Direction[i] != adir) {
    if (1==1) {
      #limit price case
      if (!onchange) {
        if (adir == "Sell") {
          if (myorder <= tabred$Price[i]) {
            print("not first sell")
            return(initprice - myorder)
          }
        } else {
          if (myorder >= tabred$Price[i]) {
            print("not first buy")
            return(myorder - initprice)
          }
        }
      } else {
        if (onchange) {
          if (adir == "Sell") {
            if (myorder <= tabred$Price[i]) {
              print("first sell")
              return(initprice - tabred$Price[i])
            }
          } else {
            if (myorder >= tabred$Price[i]) {
              print("first buy")
              return(tabred$Price[i] - initprice)
            }
          }
        }
      }
      onchange <- FALSE
    }
  }
print("cycle is over")
  #here we try to close the position:
  tabredmod <- tabred[Direction == adir, ]
  if (nrow(tabredmod) > 0) {
    if (adir == "Sell") {
      return(initprice - tabredmod$Price[nrow(tabredmod)])
    } else {
      return(tabredmod$Price[nrow(tabredmod)] - initprice)
    }
  } else {
    #return(NA)
    if (nrow(tabred) == 0) {return(NA)}
    if (adir == "Sell") {
      return(initprice - max(tabred$Price))
    } else {
      return(min(tabred$Price) - initprice)
    }
  }
}
CheckHedgeAlgoDeal <- function(deal, tabred, initslippage, T, adate) {
  adir <- deal$Direction[1]
  initprice <- deal$InitPrice[1]
  print(adir)
  print(initslippage)
  print(initprice)
#  if (adir == "Buy") {
#    ind0 <- which.max(tabred$Ask == deal$Price[1])
#    tabred <- tabred[ind0:nrow(tabred), ]
#  } else {
#    ind0 <- which.max(tabred$Bid == deal$Price[1])
#    tabred <- tabred[ind0:nrow(tabred), ]
#  }
  myorder <- initprice + ifelse(adir == "Buy", 1, -1)*initslippage
  for (i in 1:nrow(tabred)) {
    if (adir == "Buy") {
      if (tabred$Ask[i] <= myorder) {
        return(tabred$Ask[i] - initprice)
      }
      if (tabred$Ask[i] - initprice >= 50) {
        return(tabred$Ask[i] - initprice)
      }
    } else {
      if (tabred$Bid[i] >= myorder) {
        return(initprice - tabred$Bid[i])
      }
      if (initprice - tabred$Bid[i] >= 50) {
        return(initprice - tabred$Bid[i])
      }
    }
    if ((i-1) %% T == 0) {
      myorder <- myorder + ifelse(adir == "Buy", 1, -1)*initslippage
    }
  }
  i <- nrow(tabred)
  if (adir == "Buy") {
    return(tabred$Ask[i] - initprice)
  } else {
    return(initprice - tabred$Bid[i])
  }
}
CheckHedgeAlgoDay <- function(deals, initslippage, T, adate) {
  print(adate)
  deals <- as.data.table(as.data.frame(deals))
  tab <- LoadBp("SIU6@FORTS", adate, adate, "Analysis2", storage.path = "D:\\", fast = TRUE)
  res <- 0
  for (j in 1:nrow(deals)) {
    atime <- as.POSIXct(deals$Time[j])
    print(atime)
    tabred <- tab[Time >= atime & Time <= as.POSIXct(atime + 10),]
    print(nrow(tabred))
    res <- res + CheckHedgeAlgoDeal(deals[j, ], tabred, initslippage, T, adate)
    print(res)
  }
res
}
HedgeWizardTicks <- function(deals, tab, filename) {
  for (initslippage in c(-5:20, 50)) {
    for (Tlen in c(10)) {
      for (critval in c(50)) {
        for (T in 1:10) {
          res <- CheckHedgeTicksAlgo(deals, initslippage, T, Tlen, critval, tab)
          write.table(as.data.frame(cbind(initslippage, T, Tlen, critval, res[1], res[2])), filename, sep = ";",
                    col.names = FALSE, row.names = FALSE, append = TRUE)
        }
      }
    }
  }
}
HedgeWizard <- function(deals) {
  for (initslippage in 1:20) {
    for (T in c(10000, 5000, 2500, 1250, 625)) {
      res <- CheckHedgeAlgo(deals, initslippage, T)
      write.table(as.data.frame(cbind(initslippage, T, res)), "hedgetest.csv", sep = ";", row.names = FALSE, append = TRUE)
    }
  }
}
