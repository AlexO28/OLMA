#requires comparison.R
MainResearcher <- function(spotfile, futfile, lastbidspot, lastaskspot, lastbidfut, lastaskfut) {
  tabspot <- GetResearchData(spotfile)
  tabfut <- GetResearchData(futfile)
  curfut <- unique(tabfut$Security)
  if (length(curfut) > 1) {
    print("More than one futures is considered!")
    curfut <- "SIM6@FORTS"
    tabfut <- tabfut[Security == curfut, ]
  }
  setkey(tabspot, "Time")
  setkey(tabfut, "Time")
  swapres <- 0
  tabspot <- tabspot[Security != "USD000TODTOM@CETS", ]
  tabspot[, Security := "USD000UTSTOM@CETS"]
#  swapdat <- ConsiderSwapDeals(as.data.frame(tabspot))
#  tabspot <- swapdat$tab
#  swapres <- swapdat$swapres*100
  spotbuysellstruct <- GetBuySellStructureByDeals(tabspot, "USD000UTSTOM@CETS")
  futbuysellstruct <- GetBuySellStructureByDeals(tabfut, curfut)
  spotsellstruct <- spotbuysellstruct$sell
  spotbuystruct <- spotbuysellstruct$buy
  futsellstruct <- futbuysellstruct$sell
  futbuystruct <- futbuysellstruct$buy
  #infact posspot, posfut, and nevyazka are not necessary
  #that is why I am not checking the corresponding piece of code
  posspot <- GetPosByBuySellStruct(spotsellstruct, spotbuystruct)
  posfut <- GetPosByBuySellStruct(futsellstruct, futbuystruct)
  if (posspot + posfut > 0) {
    ###need to sell spot
    nevyazka <- (posspot + posfut)*lastbidspot
  } else if (posspot + posfut < 0) {
    #need to buy spot
    nevyazka <- (posspot + posfut)*lastaskspot
  }
  spotplstructloc <- EstimatePLByBuySellStruct(spotsellstruct, spotbuystruct, lastbidspot, lastaskspot, FALSE)
  spotplstructglob <- EstimatePLByBuySellStruct(spotsellstruct, spotbuystruct, lastbidspot, lastaskspot, TRUE)
  futplstructloc <- EstimatePLByBuySellStruct(futsellstruct, futbuystruct, lastbidfut, lastaskfut, FALSE)
  futplstructglob <- EstimatePLByBuySellStruct(futsellstruct, futbuystruct, lastbidfut, lastaskfut, TRUE)
  plloc <- sum(spotplstructloc$val) + sum(futplstructloc$val)/1000
  plglob <- spotplstructglob$valglobal + spotplstructglob$vallocal +
    futplstructglob$valglobal/1000 + futplstructglob$vallocal/1000 - plloc
list(loc = plloc, glob = plglob, swap = swapres, nevyazka = nevyazka)
}
GetPosByBuySellStruct <- function(sellstruct, buystruct) {
  nrow(sellstruct[!sellstruct$matched, ]) - nrow(buystruct[!buystruct$matched, ])
}
GetResearchData <- function(afile) {
  tab <- fread(afile, dec = ",")
  tab[, Time := fastPOSIXct(Date, tz = "GMT")]
  tab[, Date := as.Date(Time)]
  tab[, Direction := Side]
  tab[, Volume := as.numeric(Volume)]
  tab[, Price := as.numeric(Price)]
}
ConsiderSwapDeals <- function(tab) {
  tab <- as.data.table(tab)
  if (is.null(tab$Date)) {tab[, Date := as.Date(Time)]}
  tab <- tab[Security %in% c("USD000UTSTOM@CETS", "USD000TODTOM@CETS"), ]
  tab2 <- data.table(tab[1, ])
  swapres <- 0
  for (adate in unique(tab$Date)) {
    tabred <- tab[Date == adate, ]
    inds <- which(tabred$Security == "USD000TODTOM@CETS")
    if (length(inds) == 0) {

    } else {
      indstodel <- c()
      print(inds)
      for (ind in inds) {
        print(ind)
        swapvol <- ifelse(tabred$Direction[ind] == "Sell",
                        tabred[ind, Volume],
                        -tabred[ind, Volume])
        print(c(swapvol, tabred$Price[ind]))
        swapres <- swapres + swapvol*tabred$Price[ind]
       # print(paste0("swapres", swapres))
        found <- FALSE
        if (ind > 1) {
          if (CheckDealBySwapVol(tabred[ind-1, ], swapvol)) {
            #tabred <- tabred[-c(ind, ind-1), ]
            indstodel <- c(indstodel, ind - 1)
            found <- TRUE
          }
        }
        if (!found & (ind<nrow(tabred))) {
          if (CheckDealBySwapVol(tabred[ind+1, ], swapvol)) {
            #tabred <- tabred[-c(ind, ind+1), ]
            indstodel <- c(indstodel, ind + 1)
            found <- TRUE
          }
        }
      }
      inds <- c(inds, indstodel)
      tabred <- tabred[-inds, ]
    }
    tab2 <- rbind(tab2, tabred)
  }
list(tab = as.data.frame(tab2[2:nrow(tab2), ]), swapres = swapres)
}
CheckDealBySwapVol <- function(deal, swapvol) {
  dealval <- ifelse(deal$Direction == "Sell", deal$Volume, -deal$Volume)
dealval == swapvol*100
}
EstimatePLByBuySellStruct <- function(sellstruct, buystruct, lastbid, lastask, common = TRUE) {
  if (common) {
    vallocal <- GetEstimatePLByBuySellStruct(sellstruct, buystruct, adate = NA)
    numsell <- nrow(sellstruct[!sellstruct$matched, ])
    valglobal <- ifelse(numsell > 0, sum(sellstruct$price[!sellstruct$matched])-numsell*lastask, 0)
    numbuy <- nrow(buystruct[!buystruct$matched, ])
    print(c(numsell, numbuy))
    valglobal <- valglobal + ifelse(numbuy > 0, numbuy*lastbid - sum(buystruct$price[!buystruct$matched]), 0)
    return(list(valglobal = valglobal, vallocal = vallocal))
  }
  dates <- unique(c(as.Date(sellstruct$time), as.Date(buystruct$time)))
  dates <- sort(dates)
  print(dates)
  res <- data.frame(date = dates, val = rep(NA, length(dates)))
  for (j in 1:length(dates)) {
    adate <- res$date[j]
    res$val[j] <- GetEstimatePLByBuySellStruct(sellstruct, buystruct, adate)
  }
res
}
GetEstimatePLByBuySellStruct <- function(sellstruct0, buystruct0, adate) {
  sellstruct <- as.data.table(sellstruct0)
  buystruct <- as.data.table(buystruct0)
  sellstruct <- sellstruct[(matched), ]
  buystruct <- buystruct[(matched), ]
  print(adate)
  if (!is.na(adate)) {
    sellstruct <- sellstruct[as.Date(time) == adate & as.Date(matchedtime) == adate, ]
    buystruct <- buystruct[as.Date(time) == adate & as.Date(matchedtime) == adate, ]
  }
sellstruct[, sum(price - matchedprice)] + buystruct[, sum(matchedprice - price)]
}
GetBuySellStructureByDeals <- function(tab, instrname) {
  tab <- as.data.table(tab)
  tab <- tab[Security == instrname, ]
  sellstruct <- data.frame(time = as.POSIXct(numeric(), origin=origin),
                           price = numeric(),
                           matched = logical(),
                           matchedprice = numeric(),
                           matchedtime = as.POSIXct(numeric(), origin=origin))
  buystruct <- data.frame(time = as.POSIXct(numeric(), origin=origin),
                          price = numeric(),
                          matched = logical(),
                          matchedprice = numeric(),
                          matchedtime = as.POSIXct(numeric(), origin=origin))
  for (j in 1:nrow(tab)) {
    vol <- tab$Volume[j]
    if (tab$Direction[j] == "Buy") {
      voltomatch <- nrow(sellstruct[!sellstruct$matched, ])
      if (voltomatch > 0) {
        if (voltomatch < vol) {
          buystruct <- AddDeals(buystruct, vol - voltomatch, tab$Price[j], tab$Time[j])
          sellstruct <- MatchDeals(sellstruct, voltomatch, tab$Price[j], tab$Time[j])
        } else {
          sellstruct <- MatchDeals(sellstruct, vol, tab$Price[j], tab$Time[j])
        }
      } else {
        buystruct <- AddDeals(buystruct, vol, tab$Price[j], tab$Time[j])
      }
    } else if (tab$Direction[j] == "Sell") {
      voltomatch <- nrow(buystruct[!buystruct$matched, ])
      if (voltomatch > 0) {
        if (voltomatch < vol) {
          sellstruct <- AddDeals(sellstruct, vol - voltomatch, tab$Price[j], tab$Time[j])
          buystruct <- MatchDeals(buystruct, voltomatch, tab$Price[j], tab$Time[j])
        } else {
          buystruct <- MatchDeals(buystruct, vol, tab$Price[j], tab$Time[j])
        }
      } else {
        sellstruct <- AddDeals(sellstruct, vol, tab$Price[j], tab$Time[j])
      }
    }
  }
  sellstruct$date <- as.Date(sellstruct$time)
  sellstruct$matchedtime <- as.Date(sellstruct$matchedtime)
  buystruct$date <- as.Date(buystruct$time)
  buystruct$matcheddate <- as.Date(buystruct$matchedtime)
list(sell = sellstruct, buy = buystruct)
}
AddDeals <- function(dealsstruct, vol, price, atime) {
  rbind(dealsstruct, data.frame(
    time = rep(atime, vol),
    price = rep(price, vol),
    matched = rep(FALSE, vol),
    matchedprice = rep(NA, vol),
    matchedtime = rep(as.POSIXct("2000-01-01 00:00:00", origin = origin), vol)
    ))
}
MatchDeals <- function(dealsstruct, vol, price, atime) {
  inds <- which(dealsstruct$matched == FALSE)
  inds <- inds[1:vol]
  dealsstruct$matched[inds] <- TRUE
  dealsstruct$matchedprice[inds] <- price
  dealsstruct$matchedtime[inds] <- as.POSIXct(atime, origin = origin)
dealsstruct
}
