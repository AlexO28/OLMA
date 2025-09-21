options(stringsAsFactors = FALSE)
options(digits.secs = 3)
origin <- "1970-01-01"

Sys.setenv(TZ = "GMT")

library(boot)
library(data.table)
library(fasttime)
#library(lubridate)

Main <- function() {
  params <- read.table("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\inparams.txt", sep = "=", header = FALSE)
  filename1 <- params[params[, 1] == "filename1", 2]
  filename2 <- params[params[, 1] == "filename2", 2]
  secname1 <- params[params[, 1] == "secname1", 2]
  secname2 <- params[params[, 1] == "secname2", 2]
  src <- params[params[, 1] == "src", 2]
  comparewith <- params[params[, 1] == "comparewith", 2]
  mode <- params[params[, 1] == "mode", 2]
  writefile1 <- params[params[, 1] == "datfile1", 2]
  writefile2 <- params[params[, 1] == "datfile2", 2]

  if (mode == "studyone") {
    if (src == "Quick") {
      dat <- GetDataFromQuick(filename1, filename2, secname1, secname2)
    } else {
      dat <- GetData(filename1, filename2, secname1, secname2)
    }
    if (comparewith == "Deals") {
      deals <- HedgeComparison2(dat)
    } else {
      deals <- HedgeComparison4(dat)
    }
    mtab <- GetHedgeDiff(dat, deals)
    write.table(data.frame(diff = mtab$tab$diff), 
                paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile1), sep = ",",
                row.names = FALSE, col.names = FALSE)
    rrr <- Analyze(mtab)
  } else {
    rrr <- Compare2Traders(writefile1, writefile2)
  }
}
DailyCallFromRValutaMain <- function() {
  res <- data.frame(trader = character(), type = character(), intervaltype = numeric(), left = numeric(), right = numeric(), len = numeric())
  res <- rbind(res, DailyCallFromRValuta(0))
  res <- rbind(res, DailyCallFromRValuta(1))  
  res <- rbind(res, DailyCallFromRValuta(2))
  res <- rbind(res, DailyCallFromRValuta(3))

  resdeals <- res[res$type == "Deals", ]
  resorders <- res[res$type == "Orders", ]
  resdeals$type <- NULL
  resorders$type <- NULL

list(deals = reshape(resdeals, direction = "wide", timevar = "intervaltype", idvar = "trader"),
     orders = reshape(resorders, direction = "wide", timevar = "intervaltype", idvar = "trader"))
}
DailyCallFromRValuta <- function(intervaltype) {
  secname1 <- "USD000UTSTOM@CETS"
  secname2 <- "SIZ5@FORTS"
  src <- "Metroplex"
  res <- data.frame(trader = character(), type = character(), intervaltype = numeric(), left = numeric(), right = numeric(), len = numeric())
  confint <- MAIN("D:\\ValuteTraders\\FirstHorseman\\dealsMB0177809279.csv",
       "D:\\ValuteTraders\\FirstHorseman\\dealsR0L0LL0.csv",
       secname1, secname2, src, "Deals", "studyone", "horsemandeals.csv", NA, intervaltype=intervaltype)
  print(c("HFT", "deals", intervaltype))
  print(confint)
  res <- rbind(res, data.frame(trader = "HFT", type = "Deals", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  confint <- MAIN("D:\\ValuteTraders\\FirstHorseman\\dealsMB0177809279.csv",
       "D:\\ValuteTraders\\FirstHorseman\\ordersR0L0LL0.csv",
       secname1, secname2, src, "Orders", "studyone", "horsemanorders.csv", NA, intervaltype=intervaltype)
  print(c("HFT", "orders", intervaltype))
  res <- rbind(res, data.frame(trader = "HFT", type = "Orders", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
if (TRUE) {
  confint <- MAIN("D:\\ValuteTraders\\Kolya\\dealsMB0177810005.csv", 
       "D:\\ValuteTraders\\Kolya\\dealsR0L0L16.csv",
       secname1, secname2, src, "Deals", "studyone", "kolyadeals.csv", NA, intervaltype=intervaltype)
  print(c("kolya", "deals", intervaltype))
  res <- rbind(res, data.frame(trader = "Kolya", type = "Deals", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  confint <- MAIN("D:\\ValuteTraders\\Kolya\\dealsMB0177810005.csv", 
       "D:\\ValuteTraders\\Kolya\\ordersR0L0L16.csv",
       secname1, secname2, src, "Orders", "studyone", "kolyaorders.csv", NA, intervaltype=intervaltype)
  print(c("kolya", "orders", intervaltype))
  res <- rbind(res, data.frame(trader = "Kolya", type = "Orders", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
}
if (TRUE) {
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemandeals.csv", "kolyadeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemanorders.csv", "kolyaorders.csv")
}
if (TRUE) {
  confint <- MAIN("D:\\ValuteTraders\\Igor\\dealsMB0177809359.csv", 
       "D:\\ValuteTraders\\Igor\\dealsR0L0L13.csv",
       secname1, secname2, src, "Deals", "studyone", "igordeals.csv", NA, intervaltype=intervaltype)
  print(c("igor", "deals", intervaltype))
  res <- rbind(res, data.frame(trader = "Igor", type = "Deals", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
}
  confint <- MAIN("D:\\ValuteTraders\\Igor\\dealsMB0177809359.csv", 
       "D:\\ValuteTraders\\Igor\\ordersR0L0L13.csv",
       secname1, secname2, src, "Orders", "studyone", "igororders.csv", NA, intervaltype=intervaltype)
  print(c("igor", "orders", intervaltype))
  res <- rbind(res, data.frame(trader = "Igor", type = "Orders", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
if (TRUE) {
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemandeals.csv", "igordeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemanorders.csv", "igororders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "igordeals.csv", "kolyadeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "igororders.csv", "kolyaorders.csv")
}
  MAIN("D:\\ValuteTraders\\SecondHorseman\\dealsMB0177808169.csv",
       "D:\\ValuteTraders\\SecondHorseman\\dealsR0L0L00.csv", 
       secname1, secname2, src, "Deals", "studyone", "horsemantwodeals.csv", NA, intervaltype=intervaltype)
  confint <- MAIN("D:\\ValuteTraders\\SecondHorseman\\dealsMB0177808169.csv",
       "D:\\ValuteTraders\\SecondHorseman\\ordersR0L0L00.csv", 
       secname1, secname2, src, "Orders", "studyone", "horsemantwoorders.csv", NA, intervaltype=intervaltype)
  print(c("oleg", "orders", intervaltype))
  res <- rbind(res, data.frame(trader = "Oleg", type = "Orders", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
if (TRUE) {
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemandeals.csv", "horsemantwodeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemanorders.csv", "horsemantwoorders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemantwodeals.csv", "kolyadeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemantwoorders.csv", "kolyaorders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemantwodeals.csv", "igordeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemantwoorders.csv", "igororders.csv")
}
if (intervaltype != 3) {
  MAIN("D:\\ValuteTraders\\Ashot\\dealsMB0177809432.csv", 
       "D:\\ValuteTraders\\Ashot\\dealsR0L0LL3.csv",
       secname1, secname2, src, "Deals", "studyone", "ashotdeals.csv", NA, intervaltype=intervaltype)
  confint <- MAIN("D:\\ValuteTraders\\Ashot\\dealsMB0177809432.csv", 
       "D:\\ValuteTraders\\Ashot\\ordersR0L0LL3.csv",
       secname1, secname2, src, "Orders", "studyone", "ashotorders.csv", NA, intervaltype=intervaltype)
  print(c("ashot", "orders", intervaltype))
  res <- rbind(res, data.frame(trader = "Ashot", type = "Orders", intervaltype = intervaltype, left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemandeals.csv", "ashotdeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemanorders.csv", "ashotorders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "ashotdeals.csv", "kolyadeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "ashotorders.csv", "kolyaorders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "ashotdeals.csv", "igordeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "ashotorders.csv", "igororders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "ashotdeals.csv", "horsemantwodeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "ashotorders.csv", "horsemantwoorders.csv")  
}
res
}
DailyCallFromRStocks <- function() {
  eps <- 50
  secname1 <- "SBER@TQBR"
  secname2 <- "SRZ5@FORTS"
  src <- "Metroplex"
  res <- data.frame(trader = character(), type = character(), intervaltype = numeric(), left = numeric(), right = numeric(), len = numeric())

  weights1 <- c(100, 10)
  weights2 <- c(10, 100)
  
  if (TRUE) {
    confint <- MAIN("D:\\ValuteTraders\\Elvina\\dealsR0L0100.csv",
       "D:\\ValuteTraders\\Elvina\\dealsL01+00000F00.csv",
      secname2, secname1, src, "Deals", "studyone", "elvinadealsalt.csv", NA, weights = weights1, eps = eps)
    res <- rbind(res, data.frame(trader = "Elvina", type = "Deals", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
    confint <- MAIN("D:\\ValuteTraders\\Elvina\\dealsR0L0100.csv",
       "D:\\ValuteTraders\\Elvina\\ordersL01+00000F00.csv",
       secname2, secname1, src, "Orders", "studyone", "elvinaordersalt.csv", NA, weights = weights1, eps = eps)
    res <- rbind(res, data.frame(trader = "Elvina", type = "Orders", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  }
  if (FALSE) {
  confint <- MAIN("D:\\ValuteTraders\\Elvina\\dealsL01+00000F00.csv",
       "D:\\ValuteTraders\\Elvina\\dealsR0L0100.csv",
       secname1, secname2, src, "Deals", "studyone", "elvinadeals.csv", NA, weights = weights2, eps = eps)
  res <- rbind(res, data.frame(trader = "Elvina", type = "Deals", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  confint <- MAIN("D:\\ValuteTraders\\Elvina\\dealsL01+00000F00.csv",
       "D:\\ValuteTraders\\Elvina\\ordersR0L0100.csv",
       secname1, secname2, src, "Orders", "studyone", "elvinaorders.csv", NA, weights = weights2, eps = eps)
  res <- rbind(res, data.frame(trader = "Elvina", type = "Orders", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  }
  confint <- MAIN("D:\\ValuteTraders\\ThirdHorseman\\dealsR0L0L17.csv",
       "D:\\ValuteTraders\\ThirdHorseman\\dealsL08+00000F05.csv",
       secname2, secname1, src, "Deals", "studyone", "horsemanthreedeals.csv", NA, weights = weights1, eps = eps)
  res <- rbind(res, data.frame(trader = "Horseman3", type = "Deals", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  confint <- MAIN("D:\\ValuteTraders\\ThirdHorseman\\dealsR0L0L17.csv",
       "D:\\ValuteTraders\\ThirdHorseman\\ordersL08+00000F05.csv",
       secname2, secname1, src, "Orders", "studyone", "horsemanthreeorders.csv", NA, weights = weights1, eps = eps)
  res <- rbind(res, data.frame(trader = "Horseman3", type = "Orders", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "elvinadeals.csv", "horsemanthreedeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "elvinaorders.csv", "horsemanthreeorders.csv")
  if (FALSE) { 
    MAIN("D:\\ValuteTraders\\ThirdHorseman\\dealsL08+00000F05.csv",
       "D:\\ValuteTraders\\ThirdHorseman\\dealsR0L0L17.csv",
       secname1, secname2, src, "Deals", "studyone", "horsemanthreedealsalt.csv", NA, weights = weights2, eps = eps)
    MAIN("D:\\ValuteTraders\\ThirdHorseman\\dealsL08+00000F05.csv",
       "D:\\ValuteTraders\\ThirdHorseman\\ordersR0L0L17.csv",
       secname1, secname2, src, "Orders", "studyone", "horsemanthreeordersalt.csv", NA, weights = weights2, eps = eps)
  }
  confint <- MAIN("D:\\ValuteTraders\\FourthHorseman\\dealsR0L0L18.csv",
       "D:\\ValuteTraders\\FourthHorseman\\dealsL09+00000F06.csv",
       secname2, secname1, src, "Deals", "studyone", "horsemanfourdeals.csv", NA, weights = weights1, eps = eps)
  res <- rbind(res, data.frame(trader = "Horseman4", type = "Deals", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  confint <- MAIN("D:\\ValuteTraders\\FourthHorseman\\dealsR0L0L18.csv",
       "D:\\ValuteTraders\\FourthHorseman\\ordersL09+00000F06.csv",
       secname2, secname1, src, "Orders", "studyone", "horsemanfourorders.csv", NA, weights = weights1, eps = eps)
  res <- rbind(res, data.frame(trader = "Horseman4", type = "Orders", left = 1000*confint[1], right = 1000*confint[2], len = confint[3]))
  if (FALSE) {
    MAIN("D:\\ValuteTraders\\FourthHorseman\\dealsL09+00000F06.csv",
       "D:\\ValuteTraders\\FourthHorseman\\dealsR0L0L18.csv",
       secname1, secname2, src, "Deals", "studyone", "horsemanfourdealsalt.csv", NA, weights = weights2, eps = eps)
    MAIN("D:\\ValuteTraders\\FourthHorseman\\dealsL09+00000F06.csv",
       "D:\\ValuteTraders\\FourthHorseman\\ordersR0L0L18.csv",
       secname1, secname2, src, "Orders", "studyone", "horsemanfourordersalt.csv", NA, weights = weights2, eps = eps)
  }
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "elvinadeals.csv", "horsemanfourdeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "elvinaorders.csv", "horsemanfourorders.csv")
  MAIN(NA, NA, NA, NA, NA, "Deals", "studytwo", "horsemanthreedeals.csv", "horsemanfourdeals.csv")
  MAIN(NA, NA, NA, NA, NA, "Orders", "studytwo", "horsemanthreeorders.csv", "horsemanfourorders.csv")
res
}
MAIN <- function(filename1, filename2, secname1, secname2, src, comparewith, mode, writefile1, writefile2, weights = NA, eps = 0.1, intervaltype = 0) {
 if (mode == "studyone") {
    if (src == "Quick") {
      dat <- GetDataFromQuick(filename1, filename2, secname1, secname2, weights = weights)
    } else {
      dat <- GetData(filename1, filename2, secname1, secname2, weights = weights, intervaltype = intervaltype)
    }
    if (comparewith == "Deals") {
      deals <- HedgeComparison2(dat, eps = eps)
    } else {
      deals <- HedgeComparison4(dat, eps = eps)
    }
print(summary(deals))
    mtab <- GetHedgeDiff(dat, deals)

    write.table(data.frame(diff = mtab$tab$diff), 
                paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile1), sep = ",",
                row.names = FALSE, col.names = FALSE)
    rrr <- Analyze(mtab)
  } else {
    rrr <- Compare2Traders(writefile1, writefile2)
  }
rrr
}
Compare2Traders <- function(writefile1, writefile2) {
  dat1 <- read.table(paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile1), header = FALSE, sep = ",")
  dat2 <- read.table(paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile2), header = FALSE, sep = ",")
  dat1 <- dat1[, 1]
  dat2 <- dat2[, 1]
  LOG <- paste0("log_", Sys.Date(), ".txt")
  filename <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", LOG)
  write.table(paste("Comparing data from", writefile1, "and", writefile2), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  res1 <- wilcox.test(dat1, dat2, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean difference between 2 samples is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval of mean difference between 2 samples is", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
  dat1 <- dat1[dat1 < quantile(dat1, 0.95)]
  dat2 <- dat2[dat2 < quantile(dat2, 0.95)]
  res1 <- wilcox.test(dat1, dat2, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean difference between 2 samples without of top outliers is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval of mean difference between 2 samples without of top outliers is", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)  
}
GetDataFromQuick <- function(filename1, filename2, secname1, secname2, weights = NA) {
  if (is.na(weights[1])) {weights <- c(1, 1)}
  deals <- GetDataFromQuickPart(filename1, secname1, weight = weights[1])
  orders <- GetDataFromQuickPart(filename2, secname2, weight = weights[2])
list(deals = deals, orders = orders)
}
GetDataFromQuickPart <- function(filename, secname, date = NA, weight = 1) {
print(filename)
print(secname)
  tab <- read.table(filename, header = TRUE, sep = ";", comment.char = "")
  tab <- tab[!is.na(tab$Time) & !is.na(tab$MS), ]
  print(head(tab))
  tab$MS <- tab$MS %/% 1000
  abnormal <- which(tab$MS < 10)
  ###tab$MS <- ifelse(tab$MS < 10, 10, tab$MS)
  tab$MS[-abnormal] <- ifelse(tab$MS[-abnormal] >= 100, as.character(tab$MS[-abnormal]), paste0("0", as.character(tab$MS[-abnormal])))
  tab$MS[abnormal] <- paste0("00", as.character(tab$MS[abnormal]))
  if (is.na(date)) {
    date <- Sys.Date()
  }
  print(date)
  tab$Date <- paste0(date, " ", tab$Time, ".", tab$MS)
  tab$Volume <- weight*ifelse(tab$Side == "Buy", 1, -1)*tab$Volume
print("hee")
  tab$Date <- as.POSIXct(as.character(tab$Date), origin = origin)
print(head(tab))
  tab <- tab[order(tab$Date), ]
  tab <- tab[tab$Security == secname, ]
  tab$id <- 1:nrow(tab)  
  tab$Time <- tab$Date
  tab <- as.data.table(tab)
print(head(tab))
setkey(tab, "Time")
}
GetTimesByIntervaltype <- function(intervaltype) {
  if (intervaltype == 0) {
    timestart <- 10
    timeend <- 23
  } else if (intervaltype == 1) {
    timestart <- 15
    timeend <- 18
  } else if (intervaltype == 2) {
    timestart <- 10
    timeend <- 14
  } else if (intervaltype == 3) {
    timestart <- 19
    timeend <- 23
  }
c(timestart, timeend)
}
GetDataFromMetroplexPart <- function(filename, secname, weight = 1, intervaltype = 0, dates = NA) {
  times <- GetTimesByIntervaltype(intervaltype)
  timestart <- times[1]
  timeend <- times[2]
  tab <- fread(filename)
  setkey(tab, "Security")
  tab <- tab[list(secname)]
  tab[, Time := fastPOSIXct(Date, tz = "GMT")]
  setkey(tab, "Time")
  tab <- tab[as.POSIXlt(Time)$hour >= timestart & as.POSIXlt(Time)$hour <= timeend, ]
  #tab <- tab[order(tab$Date), ]
  #tab$Volume <- weight*tab$Volume*ifelse(tab$Side == "Buy", 1, -1)
  #tab$Date <- as.POSIXct(tab$Date, origin = origin)
  tab[, ":="(Volume = weight*Volume*ifelse(Side == "Buy", 1, -1), Date = as.Date(Time))]
  if (!is.na(dates)) {
    print(dates)
    tab <- tab[as.Date(tab$Date) == as.Date(dates), ]
  }
  tab[, id := 1:nrow(tab)]
tab
}
GetData <- function(filename1, filename2, secname1, secname2, weights = NA, intervaltype = 0, dates = NA) {
  if (is.na(weights[1])) {weights <- c(1, 1)}
  deals <- GetDataFromMetroplexPart(filename1, secname1, weight = weights[1], intervaltype = intervaltype, dates = dates)
  #deals <- deals[deals$Date >= as.POSIXct("2015-12-02 18:18:00") & deals$Date <= as.POSIXct("2015-12-02 18:21:00"), ]
  orders <- GetDataFromMetroplexPart(filename2, secname2, weight = weights[2], intervaltype = intervaltype, dates = dates)
list(deals = deals, orders = orders)
}
HedgeComparison1 <- function(dat, eps = 0.1) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- FALSE
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(j)
    time0 <- as.POSIXct(deals$Time[j])
    ordersred <- orders[(!orders$matched) & (orders$Time >= time0) & (orders$Time <= time0 + 60), ]
    print(nrow(ordersred))    
    if (nrow(ordersred) > 0) {
      vol <- deals$Volume[j]
      ordersred2 <- ordersred[abs(ordersred$Volume - (-vol))<=eps, ]
      if (nrow(ordersred2) > 0) {
        oid <- ordersred2$id[1]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id == oid] <- TRUE
      } else {print("skipping")}
    }
  }
deals
}
HedgeComparison3 <- function(dat) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- 0
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(paste("starting", j))
    time0 <- as.POSIXct(deals$Time[j])
    vol <- deals$Volume[j]
    ordersred <- orders[(orders$Time >= time0) & (orders$Time <= time0 + 60) & 
                          (orders$matched + vol <= abs(orders$Volume) + 0.1) &
                          (orders$Volume*vol < 0), ]
    if (nrow(ordersred) > 0) {
      oid <- ordersred$id[1]
      deals$hedgeid[j] <- oid
      orders$matched[orders$id == oid] <- orders$matched[orders$id == oid] + abs(vol)
    } else {print("skipping")}
  }
deals
}
HedgeComparison2 <- function(dat, eps = 0.1) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- FALSE
  deals$hedgeid <- NA 

  for (j in 1:nrow(deals)) {
    time0 <- as.POSIXct(deals$Time[j])
     ordersred <- orders[(!orders$matched) & (orders$Time >= time0) & (orders$Time <= time0 + 60), ]
    if (nrow(ordersred) > 0) {
      vol <- deals$Volume[j]
      finpos <- which(ordersred$Volume*vol > 0)
      if (length(finpos) == 0) {
        finpos <- nrow(ordersred)
      } else {
        finpos <- finpos[1]
      }
      ordersred <- ordersred[1:finpos, ]
      ordersred$cs <- cumsum(ordersred$Volume)
      chosenpos <- which(abs(ordersred$cs - (-vol))<=eps)
      if (length(chosenpos) > 0) {
        chosenpos <- chosenpos[1]
        oidmin <- ordersred$id[1]
        oid <- ordersred$id[chosenpos]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id >= oidmin & orders$id <= oid] <- TRUE
      } else {}
    } else {}
  }
deals
}
HedgeComparison4 <- function(dat, eps = 0.1) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- 0
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    time0 <- as.POSIXct(deals$Time[j])
    vol <- deals$Volume[j]
    ordersred <- orders[(orders$Time >= time0) & (orders$Time <= time0 + 60), ]
    if (nrow(ordersred) > 0) {
      ordersred <- ordersred[1, ]
      if (ordersred$Volume[1]*vol < 0 & ordersred$matched + abs(vol) <= abs(ordersred$Volume) + 0.1 + eps) {
        oid <- ordersred$id[1]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id == oid] <- orders$matched[orders$id == oid] + abs(vol)
      } else {}
    }
  }
deals
}
GetHedgeDiff <- function(dat, deals) {
  orders <- dat$orders
  tot <- nrow(deals)
  deals <- deals[!is.na(deals$hedgeid), ]
  rel1 <- nrow(deals)/tot
  names(orders) <- tolower(names(orders))
  mtab <- cbind(deals, orders[deals$hedgeid, ]) 
  mtab$diff <- as.numeric(mtab$time - mtab$Time, units = "secs")
list(tab = mtab, val = rel1)
}
Analyze <- function(info) {
  tab <- info$tab
  val <- info$val
  print(val)
#  if (nrow(tab) < 10) {
#    return(rep(NA, 3))
#  }
  atime <- Sys.time()
  atime <- gsub(" ", "_", as.character(atime))
  atime <- gsub(":", "-", atime)

  LOG <- paste0("log_", Sys.Date(), ".txt")
  filename <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", LOG)  
  write.table("Started analysis", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Using", 100*val, "percent of available data"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds is from", confint[1], "to", confint[2]), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  pict <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\pict_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Summary is"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(rbind(as.numeric(summary(tab$diff)), c()), filename, row.names = FALSE, col.names = FALSE, append = TRUE)

  tab <- tab[tab$diff < as.numeric(quantile(tab$diff, 0.95)), ]
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate without of top outliers in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds witout of top outliers is from", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
  pict <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\pictrobust_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Summary without of top outliers is"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(rbind(as.numeric(summary(tab$diff)), c()), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  ###here we apply bootstrapping:
  bootstrapresult <- BootStrapMeanEstimate(tab$diff)
  write.table(paste("Bootstrap estimate", bootstrapresult$estimate), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Bootstrap confidence interval: from", bootstrapresult$conf.int[1], 
                    "to", bootstrapresult$conf.int[2]), 
                    filename, row.names = FALSE, col.names = FALSE, append = TRUE)   
  print(c(bootstrapresult$conf.int[1], bootstrapresult$conf.int[2], nrow(tab)))
c(bootstrapresult$conf.int[1], bootstrapresult$conf.int[2], nrow(tab))
}
BootStrapMeanEstimate <- function(vec) { 
  samplemean <- function(x, d) {
    mean(x[d])
  }

  bootdat <- boot(vec, samplemean, 10000)
  resvec <- bootdat$t
  alpha <- 0.025
  resvec <- resvec[resvec > as.numeric(quantile(resvec, alpha)) & resvec < as.numeric(quantile(resvec, 1-alpha))]
list(conf.int = c(min(resvec), max(resvec)), estimate = mean(resvec))
}
BootStrapMedianEstimate <- function(vec) {
  samplemedian <- function(x, d) {
    median(x[d])
  }

  bootdat <- boot(vec, samplemedian, 10000)
  resvec <- bootdat$t
  alpha <- 0.025
  resvec <- resvec[resvec > as.numeric(quantile(resvec, alpha)) & resvec < as.numeric(quantile(resvec, 1-alpha))]
list(conf.int = c(min(resvec), max(resvec)), estimate = mean(resvec))
}
MultiSlippageWizard <- function(intervaltype, datestart, dateend, skiptoanalysis = FALSE) {
  #here we get data
  tab <- NULL
  if (intervaltype == 0 & (!skiptoanalysis)) {
    for (date0 in seq(datestart, dateend, 1)) {
      date0 <- as.Date(date0, origin = origin)
      rm(tab)
      tab <- GetFutQuotes("SIZ5@FORTS", date0, date0)
      setkey(tab, "Time")
      print(nrow(tab))
      if (nrow(tab) == 0) {next}
      print(head(tab))
      delme <- MainSlippageWizard(tab, intervaltype, appendmode = TRUE, dates = date0)
    }
  }
  res <- data.frame(trader = character(), intervaltype = numeric(), left = numeric(), right = numeric(), 
                    len = numeric(), medleft = numeric(), medright = numeric())
  traderres <- AnalysisWizard("horsemanone", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "horsemanone", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("kolya", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "kolya", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("igor", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "igor", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("horsemantwo", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "oleg", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("ashot", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "ashot", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
res
}
MultiSlippageWizardStocks <- function(intervaltype, datestart, dateend, skiptoanalysis = FALSE) {
  tab <- NULL
  if (intervaltype == 0 & (!skiptoanalysis)) {
    print(seq(datestart, dateend, 1))
    for (date0 in seq(datestart, dateend, 1)) {
      date0 <- as.Date(date0, origin = origin)
      print(date0)
      rm(tab)
      tab <- GetFutQuotes("SBER@TQBR", date0, date0)
      setkey(tab, "Time")
      print(nrow(tab))
      if (nrow(tab) == 0) {next}
      print(head(tab))
      delme <- MainSlippageWizardStocks(tab, intervaltype, appendmode = TRUE, dates = date0)
      print("next")
    }
  }
  res <- data.frame(trader = character(), intervaltype = numeric(), left = numeric(), right = numeric(), 
                    len = numeric(), medleft = numeric(), medright = numeric())
  traderres <- AnalysisWizard("elvina", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "elvina", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("horsemanthree", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "horsemanthree", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
  traderres <- AnalysisWizard("horsemanfour", intervaltype = intervaltype)
  res <- rbind(res, data.frame(trader = "horsemanfour", intervaltype = intervaltype, left = traderres[1], right = traderres[2],
                               len = traderres[3], medleft = traderres[4], medright = traderres[5]))
res
}
AnalysisWizard <- function(trader, intervaltype = 0) {
  filename <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\slippage", trader, ".txt")
  print(filename)
  slippagevec <- read.table(filename, header = FALSE, sep = ",")
  slippagevec <- data.frame(time = slippagevec[, 1], slippage = slippagevec[, 2])
  if (intervaltype > 0) {
    times <- GetTimesByIntervaltype(intervaltype)
    timestart <- times[1]
    timeend <- times[2]
    slippagevec <- slippagevec[as.POSIXlt(slippagevec$time)$hour >= timestart & as.POSIXlt(slippagevec$time)$hour <= timeend, ]
  }
  slippagevec <- slippagevec$slippage
  slippagevec <- slippagevec[slippagevec <= as.numeric(quantile(slippagevec, 0.95)) & 
                             slippagevec >= as.numeric(quantile(slippagevec, 0.05))]
  bootstrapresult <- BootStrapMeanEstimate(slippagevec)
  bootstrapresult2 <- BootStrapMedianEstimate(slippagevec)
c(bootstrapresult$conf.int[1], bootstrapresult$conf.int[2], length(slippagevec), bootstrapresult2$conf.int[1], bootstrapresult2$conf.int[2])
}
MainSlippageWizard <- function(hedgedat, intervaltype, appendmode = FALSE, dates = NA) {
  secname1 <- "USD000UTSTOM@CETS"
  secname2 <- "SIZ5@FORTS"
  src <- "Metroplex"
  res <- data.frame(trader = character(), intervaltype = numeric(), left = numeric(), right = numeric(), 
                    len = numeric(), medleft = numeric(), medright = numeric())
  temp <- SlippageWizard("D:\\ValuteTraders\\FirstHorseman\\dealsMB0177809279.csv",
                         "D:\\ValuteTraders\\FirstHorseman\\dealsR0L0LL0.csv", secname1, secname2, src, hedgedat, "horsemanone", intervaltype = intervaltype, appendmode = appendmode, dates = dates)
  res <- rbind(res, data.frame(trader = "HFT", intervaltype = intervaltype, left = temp[1], right = temp[2], 
                               len = temp[3], medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\Kolya\\dealsMB0177810005.csv",
                         "D:\\ValuteTraders\\Kolya\\dealsR0L0L16.csv", secname1, secname2, src, hedgedat, "kolya", intervaltype = intervaltype, appendmode = appendmode, dates = dates)
  res <- rbind(res, data.frame(trader = "Kolya", intervaltype = intervaltype, left = temp[1], right = temp[2],
                               len = temp[3], medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\Igor\\dealsMB0177809359.csv",
                         "D:\\ValuteTraders\\Igor\\dealsR0L0L13.csv", secname1, secname2, src, hedgedat, "igor", intervaltype = intervaltype, appendmode = appendmode, dates = dates)
  res <- rbind(res, data.frame(trader = "Igor", intervaltype = intervaltype, left = temp[1], right = temp[2],
                               len = temp[3], medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\SecondHorseman\\dealsMB0177808169.csv",
                         "D:\\ValuteTraders\\SecondHorseman\\dealsR0L0L00.csv", secname1, secname2, src, hedgedat, "horsemantwo", intervaltype = intervaltype, appendmode = appendmode, dates = dates)
  res <- rbind(res, data.frame(trader = "Oleg", intervaltype = intervaltype, left = temp[1], right = temp[2], len = temp[3],
                               medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\Ashot\\dealsMB0177809432.csv",
                         "D:\\ValuteTraders\\Ashot\\dealsR0L0LL3.csv", secname1, secname2, src, hedgedat, "ashot", intervaltype = intervaltype, appendmode = appendmode, dates = dates)
  res <- rbind(res, data.frame(trader = "Ashot", intervaltype = intervaltype, left = temp[1], right = temp[2], 
                               len = temp[3], medleft = temp[4], medright = temp[5]))
res
}
MainSlippageWizardStocks <- function(hedgedat, intervaltype, appendmode = FALSE, dates = NA) {
  secname1 <- "SBER@TQBR"
  secname2 <- "SRZ5@FORTS"
  eps <- 50
  weights <- c(100, 10)
  src <- "Metroplex"
  res <- data.frame(trader = character(), intervaltype = numeric(), left = numeric(), right = numeric(), 
                    len = numeric(), medleft = numeric(), medright = numeric())
  temp <- SlippageWizard("D:\\ValuteTraders\\Elvina\\dealsR0L0100.csv",
                         "D:\\ValuteTraders\\Elvina\\dealsL01+00000F00.csv", secname2, secname1, src, hedgedat, "elvina", intervaltype = intervaltype, appendmode = appendmode, dates = dates,
                         eps = eps, weights = weights)
  res <- rbind(res, data.frame(trader = "Elvina", intervaltype = intervaltype, left = temp[1], right = temp[2],
                               len = temp[3], medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\ThirdHorseman\\dealsR0L0L17.csv",
                         "D:\\ValuteTraders\\ThirdHorseman\\dealsL08+00000F05.csv", secname2, secname1, src, hedgedat, "horsemanthree", intervaltype = intervaltype, appendmode = appendmode, dates = dates,
                         eps = eps, weights = weights)
  res <- rbind(res, data.frame(trader = "Horseman3", intervaltype = intervaltype, left = temp[1], right = temp[2],
                               len = temp[3], medleft = temp[4], medright = temp[5]))
  temp <- SlippageWizard("D:\\ValuteTraders\\FourthHorseman\\dealsR0L0L18.csv",
                         "D:\\ValuteTraders\\FourthHorseman\\dealsL09+00000F06.csv", secname2, secname1, src, hedgedat, "horsemanfour", intervaltype = intervaltype, appendmode = appendmode, dates = dates,
                         eps = eps, weights = weights)
  res <- rbind(res, data.frame(trader = "Horseman4", intervaltype = intervaltype, left = temp[1], right = temp[2],
                               len = temp[3], medleft = temp[4], medright = temp[5]))
res
}
SlippageWizard <- function(filename1, filename2, secname1, secname2, src, hedgedat, idname, intervaltype = 0, appendmode = FALSE, dates = NA, eps = 0.1, weights = NA) {
  print("intervaltype")
  print(intervaltype)
  src <- "Metroplex"
  if (intervaltype == 0) {
    if (src == "Quick") {
      dat <- GetDataFromQuick(filename1, filename2, secname1, secname2)
    } else {
print(weights)
      dat <- GetData(filename1, filename2, secname1, secname2, intervaltype = intervaltype, dates = dates, weights = weights)
    }
    deals <- HedgeComparison2(dat, eps = eps)
    mtab <- GetHedgeDiff(dat, deals)
    mtab2 <- mtab$tab
    mtab2 <- as.data.table(mtab2)
    setkey(mtab2, "time")    

    #print(head(mtab2))
    #ids <- GetInfoByTimes(mtab2, hedgedat, "Time")
    #print("over")
    #hedgetab <- hedgedat[ids, ]
    #mtab2 <- mtab2[1:5, ]
    #sliptab <- cbind(mtab2, hedgetab)
    #print(names(sliptab))
    #print(ncol(sliptab))
    print(head(mtab2))
    print(head(hedgedat))
    sliptab <- hedgedat[mtab2, roll = TRUE, mult = "first"]

    #return(sliptab)

    slippagevec <- apply(sliptab, 1, "CalculateSlippageServe", slipnames = names(sliptab))
    names(slippagevec) <- c()
    res <- SlippageAnalysis(data.frame(time = mtab2$time, slippage = slippagevec), idname, appendmode = appendmode)
  } else {
    #we take the data from the file
    sliptab <- read.table(paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\slippage", idname, ".txt"), sep = ",", header = FALSE)
    times <- GetTimesByIntervaltype(intervaltype)
    timestart <- times[1]
    timeend <- times[2]
    sliptab <- data.frame(time = sliptab[, 1], slippage = sliptab[, 2])
    sliptab <- sliptab[as.POSIXlt(sliptab$time)$hour >= timestart & as.POSIXlt(sliptab$time)$hour <= timeend, ]
    res <- SlippageAnalysis(sliptab, idname)
  }
res
}
SlippageAnalysis <- function(slippagevec, idname, appendmode = FALSE) {
  #first we write down data about slippage
  print("ready to write down")
  write.table(slippagevec, paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\slippage", idname, ".txt"), row.names = FALSE, col.names = FALSE, append = appendmode, sep = ",")
  slippagevec <- slippagevec$slippage
  slippagevec <- slippagevec[slippagevec <= as.numeric(quantile(slippagevec, 0.95)) & 
                             slippagevec >= as.numeric(quantile(slippagevec, 0.05))]
  bootstrapresult <- BootStrapMeanEstimate(slippagevec)
  bootstrapresult2 <- BootStrapMedianEstimate(slippagevec)
c(bootstrapresult$conf.int[1], bootstrapresult$conf.int[2], length(slippagevec), bootstrapresult2$conf.int[1], bootstrapresult2$conf.int[2])
}
CalculateSlippageServe <- function(sliptab, slipnames) {
  sliptab <- as.data.frame(t(sliptab))
  names(sliptab) <- slipnames
  if (sliptab$Side == "Buy") {
    return(as.numeric(sliptab$Bid) - as.numeric(sliptab$price))
  } else if (sliptab$Side == "Sell") {
    return(as.numeric(sliptab$price) - as.numeric(sliptab$Ask))   
  } else {stop("Unknown format of Side in deals")}
}
GetFutQuotes <- function(instrname, datestart, dateend) {
  tab <- LoadBp(instrname, datestart, dateend, candle.type = "ValuteTraders", storage.path = "D:")
  if (nrow(tab)>0) {tab$id <- 1:nrow(tab)}
tab
}
###taken from MysteryOf4Numbers.R
#trades is a small data.frame, df is a large data.frame
#trades has a column named time, df has a column named id, securname is the name of time-column in df
#GetInfoByTimes returns id-columns of first relevant strings
GetInfoByTimes <- function(trades, df, securname, simplemode = 1) {
  vec <- trades$time
  #vec <- vec[1:5]
  print(length(vec))
  temp <- (sapply(vec, FUN = GetInfoByTime, df = df, securname = securname))
temp
}
GetInfoByTime <- function(time0, df, securname) {
  print(time0)
  temp <- df$id[df[, securname] <= time0 & df[, securname] >= time0 - 1]
al <- temp[length(temp)]
unlist(al)
}
###taken from qlib
# load bp from file
LoadBpFile <- function(file, only.main.session=F, start.time=NULL, end.time=NULL) {
	#df <- read.csv2(file, as.is = TRUE, na.strings="-1")
        df <- fread(file, na.strings = "-1", drop = 4)
        df[, Time := fastPOSIXct(Time, tz = "GMT")]
        date0 <- as.Date(df$Time[1])
        df[, Date := rep(date0, nrow(df))]
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
		df <- df[dates + hm(start.time) <= df$Time &
						 	df$Time < dates + hm(end.time),  ]
	}
	df
}

# take some bid-ask data from storage [one day], use hh:mm for specific time period
LoadBpDay <- function(instrument, date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp")) {
  dir <- file.path(storage.path, candle.type, instrument)
  file <- list.files(dir, pattern=as.character(date, "%Y_%m_%d"))
  if(length(file)==0)print(paste0(instrument,"!: ",as.character(date, "%Y_%m_%d")," not found"))
  if (length(file) > 0) {
    ticks <- LoadBpFile(file.path(dir, file), only.main.session, start.time, end.time)
  } else {
    ticks <- CreateBp()
  }
  ticks
}

# download bid-ask data from local storage [start.date,end.date]
LoadBp <- function(instrument, start.date, end.date, candle.type="1m", only.main.session=F, start.time=NULL, end.time=NULL, storage.path=file.path("//192.168.1.12","historical_data", "PLAZA","bp")) {
		start.date <- as.Date(start.date, origin=origin)
		end.date <- as.Date(end.date, origin=origin)
	as.data.table(
		do.call(
			rbind, lapply(as.Date(start.date:end.date, origin=origin), function(date) {
				LoadBpDay(instrument, date, candle.type, only.main.session, start.time, end.time, storage.path)
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
		stringsAsFactors = FALSE
	)
}

	