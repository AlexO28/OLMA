SetGlobalOptions <- function() {
    options(stringsAsFactors = FALSE)
    options(digits.secs = 3)
    Sys.setenv(tz = "GMT")
}
library(data.table)
library(fasttime)
library(lubridate)
MainCalculateRepos <- function(report, datestart, dateend, curswapname, curspotname, atime, nextdate) {
  tabswap <- GetSwapDataStocks(datestart, dateend, curswapname, atime)
  CalculateRepos(report, tabswap, curspotname, atime, nextdate)
}
GetSwapDataStocks <- function(datestart, dateend,
                              curswapname = "USD000TODTOM@CETS",
                              atime = "10:10:00",
                              candletype = "Data",
                              #storage.path = "\\\\192.168.1.204\\share\\People\\???????\\") {
                              storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\") {
  tabswap <- LoadBp(curswapname, datestart, dateend, candle.type = candletype, storage.path = storage.path,
                    start.time = atime, end.time = atime)
  tabswap
}
DefineFileByIdRepos <- function(optimid, mystr = "trades.csv") {
  #modified code
  mystr <- paste0("\\", mystr)
  #end of modified code
  if (optimid < 10) {
    optimfile <- paste0("0000", optimid, mystr)
    print(optimfile)
  } else if (optimid < 100) {
    optimfile <- paste0("000", optimid, mystr)
  } else if (optimid < 1000) {
    optimfile <- paste0("00", optimid, mystr)
  } else if (optimid < 10000) {
    optimfile <- paste0("0", optimid, mystr)
  } else {
    optimfile <- paste0(optimid, mystr)
  }
  optimfile
}
CalculateRepos <- function(reportpath, tabswap, curspotname, atime, nextdate) {
  print(paste0(reportpath, "optimizerResult.csv"))
  optimres <- as.data.frame(fread(paste0(reportpath, "optimizerResult.csv")))
  optimids <- optimres[, ncol(optimres)]
  vec <- c()
  for (optimid in optimids) {
    optimfile <- DefineFileByIdRepos(optimid)
    swappl <- CalculateTimeIntervalDataRepos(paste0(reportpath, "IterationsResult\\", optimfile),
                                        tabswap, curspotname, atime = atime, nextdate = nextdate)
    vec <- c(vec, swappl)
  }
  optimres$swappl <- NULL
  optimres$swappl <- vec
  optimres <- optimres[, c(ncol(optimres), 1:(ncol(optimres)-1))]
  write.table(optimres, paste0(reportpath, "optimizerResult.csv"), row.names = FALSE, sep = ";", dec = ",")
}
CalculateTimeIntervalDataRepos <- function(filepath, tabswap, curspotname, atime, nextdate) {
  print(filepath)
  tab <- fread(filepath, select = c(1, 3, 4, 5, 6))
  tab[, Time := fastPOSIXct(Time, tz = "GMT")]
  tab[, Date := as.Date(Time)]
  tab$Price <- as.numeric(gsub(",", ".", as.character(tab$Price)))
  dealsspot <- CalculatePosStocks(tab, curspotname)
  dates <- unique(c(tabswap$Date, dealsspot$Date))
  dates <- dates[order(dates)]
#  print(dates)

  pos <- 0
  posprev <- 0

  deltab <- data.frame(Date = as.Date(origin), Pos = NA, SwapVal = NA)

  if (length(dates) == 0) {return(0)}
  swappl <- 0
  mod <- GetModByInstr(curspotname)
  for (j in 1:length(dates)) {
    adate <- dates[j]
    if (j == length(dates)) {
      adatenext <- nextdate
    } else {
      adatenext <- dates[j+1]
    }
    print(as.Date(adate))
    if (!(curspotname %in% c("USD000UTSTOM@CETS", "EUR_RUB__TOM@CETS"))) {
      myspotforrepo <- (LoadBp(curspotname, start.date = adate, end.date = adate, candle.type = "Data",
                            #storage.path = "\\\\192.168.1.204\\share\\People\\???????\\",
                            storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\",
                            start.time = atime, end.time = atime))
      myspotforrepo$Bid[myspotforrepo$Bid == -1] <- NA
      myspotforrepo$Bid[myspotforrepo$Ask == -1] <- NA
    #  print(myspotforrepo)
    }
    tabswapred <- tabswap[Date == adate, ]
  #  print(tabswapred)
    if (curspotname %in% c("USD000UTSTOM@CETS", "EUR_RUB__TOM@CETS")) {
      if (pos > 0) {
        if (nrow(tabswapred) > 0) {
          swappl <- swappl - abs(pos)*tabswapred$Ask[1]
        }
      } else {
        if (nrow(tabswapred) > 0) {
          swappl <- swappl + abs(pos)*tabswapred$Bid[1]
        }
      }
      print(c(pos, swappl))
      deltab <- rbind(deltab, data.frame(Date = adate, Pos = pos, SwapVal = swappl))
    } else {
      print("in")
      print(c(posprev))
      print(tabswapred)
      print(as.numeric(adatenext) - as.numeric(adate))
      print((myspotforrepo$Ask[1]+myspotforrepo$Bid[1])/2)

      if (posprev > 0) {
        if (nrow(myspotforrepo) > 0) {
         # swappl <- swappl - abs(posprev)*(myspotforrepo$Ask[1]-myspotforrepo$Bid[1])*
        #    0.9*(1-tabswapred$Ask[1]/100)*(as.numeric(adatenext) - as.numeric(adate))
          swappl <- swappl - abs(posprev)*((myspotforrepo$Ask[1]+myspotforrepo$Bid[1])/2)*
            0.9*tabswapred$Ask[1]*(as.numeric(adatenext) - as.numeric(adate))/(100*365)
        }
       # swappl <- swappl - abs(posprev)*
      #    ifelse(is.na(myspotforrepo$Ask[1]), myspotforrepo$Bid[1],
      #                                         myspotforrepo$Ask[1])*
      #    (1 - tabswapred$Ask[1]/100)
      #  print("****")
      #  print(ifelse(is.na(myspotforrepo$Ask[1]), myspotforrepo$Bid[1],
      #               myspotforrepo$Ask[1])*
      #          (1 - tabswapred$Ask[1]/100))
      } else {
        if (nrow(myspotforrepo) > 0) {
          #swappl <- swappl + abs(posprev)*(myspotforrepo$Ask[1]-myspotforrepo$Bid[1])*
          #  0.9*(1-tabswapred$Bid[1]/100)*(as.numeric(adatenext) - as.numeric(adate))
          swappl <- swappl + abs(posprev)*((myspotforrepo$Ask[1]+myspotforrepo$Bid[1])/2)*
            0.9*tabswapred$Ask[1]*(as.numeric(adatenext) - as.numeric(adate))/(100*365)
        }
       # swappl <- swappl + abs(posprev)*
      #    ifelse(is.na(myspotforrepo$Bid[1]), myspotforrepo$Ask[1],
      #           myspotforrepo$Bid[1])*
      #    (1 - tabswapred$Bid[1]/100)
      #  print("***")
      #  print(ifelse(is.na(myspotforrepo$Bid[1]), myspotforrepo$Ask[1],
      #         myspotforrepo$Bid[1])*
      #    (1 - tabswapred$Bid[1]/100))
      }
    }
  #  print(c(swappl, posprev))
    posprev <- pos
    if (is.na(swappl)) {stop()}
    dealsspotred <- dealsspot[Date == adate, ]
    if (nrow(dealsspotred) > 0) {
      pos <- dealsspotred[nrow(dealsspotred), Pos]
    }
  }
  print("***")
  print(swappl*mod/10)
  print(mod)
  print(swappl)
  #stop()
 # View(deltab)
#  stop()
  if (curspotname %in% c("USD000UTSTOM@CETS", "EUR_RUB__TOM@CETS")) {print(mod*swappl); return(mod*swappl)} else {return(swappl*mod/10)}
}
GetModByInstr <- function(instrname) {
  if (instrname == "USD000UTSTOM@CETS" | instrname == "EUR_RUB__TOM@CETS") {
    return(1000)
  } else if (instrname == "SBER@TQBR" | instrname == "GAZP@TQBR") {
    return(100)
  } else if (instrname == "LKOH@TQBR") {
    return(10)
  } else if (instrname == "VTBR@TQBR") {
    return(100000)
  } else {
    return(NA)
  }
}
CalculatePosStocks <- function(deals, instrname) {
  deals <- as.data.table(as.data.frame(deals))
  deals <- deals[Security == instrname, ]
  deals[, Pos := cumsum(Volume*ifelse(Direction == "Buy", 1, -1))]
  deals[, Rpl := cumsum(Volume*ifelse(Direction == "Sell", 1, -1)*Price)]
  setkey(deals, "Time")
}

################

LoadBpFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL, fast = FALSE) {
  #df <- read.csv2(file, as.is = TRUE, na.strings="-1")
  df <- fread(file, na.strings = "-1", drop = 4)
  df[, Time := fastPOSIXct(Time, tz = "GMT")]
  date0 <- as.Date(df$Time[1])
  df[, Date := rep(date0, nrow(df))]
  if (fast) {return(df)}
  if(only.main.session)
  {
    # only main session = T => use 10:00-18:45
    dates <- df$Date
    df <- df[dates + hm("10:00") <= df$Time &
               df$Time < dates + hm("18:45"),  ]
  }
  else if(!is.null(start.time)){
    # only main session = F and start.time != NULL => use [start.time - end.time]
    dates <- df$Date
    df <- as.data.frame(df)
    print(start.time)
    print(end.time)
    df <- df[dates + hms(start.time) <= df$Time &
               df$Time <= dates + hms(end.time),  ]
    df <- as.data.table(df)
  }
  df[, Bid := as.numeric(sub(',', '.', Bid))]
  df[, Ask := as.numeric(sub(',', '.', Ask))]
  df[, Close := (Bid + Ask)/2]
  df
}

# take some bid-ask data from storage [one day], use hh:mm for specific time period
LoadBpDay <- function(instrument, date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp"),
                      fast = FALSE) {
  dir <- file.path(storage.path, candle.type, instrument)
  file <- list.files(dir, pattern=as.character(date, "%Y_%m_%d"))

  print(dir)
  print(file)

  if(length(file)==0)print(paste0(instrument,"!: ",as.character(date, "%Y_%m_%d")," not found"))
  if (length(file) > 0) {
    ticks <- LoadBpFile(file.path(dir, file), only.main.session, start.time, end.time, fast = fast)
  } else {
    ticks <- CreateBp()
  }
  ticks
}

# download bid-ask data from local storage [start.date,end.date]
LoadBp <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp"),
                   fast = FALSE) {
  SetGlobalOptions()
  start.date <- as.Date(start.date, origin=origin)
  end.date <- as.Date(end.date, origin=origin)
  as.data.table(
    do.call(
      rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
        LoadBpDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path, fast = fast)
      })
    )
  )
}
CreateBp <- function() {
  data.frame(
    Time=as.POSIXct(numeric(), origin=origin),
    Bid=numeric(),
    Ask=numeric(),
    Date=as.Date(numeric(), origin=origin),
    Close = numeric(),
    stringsAsFactors = FALSE
  )
}

