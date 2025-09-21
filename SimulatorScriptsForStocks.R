GetQuotesTabStocks <- function(datestart, dateend, closetime = "23:40:00",
                               curspotname = "USD000UTSTOM@CETS",
                               curfutname = "SIZ5@FORTS",
                               candletype = "Data",
                               storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\") {
  tabdol <- LoadBp(curspotname, datestart, dateend, candle.type = candletype, storage.path = storage.path,
                   start.time = closetime, end.time = closetime)
  tabfut <- LoadBp(curfutname, datestart, dateend, candle.type = candletype, storage.path = storage.path,
                   start.time = closetime, end.time = closetime)
  list(dol = tabdol, fut = tabfut)
}
CalculateOptimResults <- function(reportpath, quotestab, curspotname, curfutname,
                                  tabswap, bank=NA) {
  optimres <- as.data.frame(fread(paste0(reportpath, "optimizerResult.csv")))
  optimids <- optimres[, ncol(optimres)]
  cumres <- data.frame(id = numeric(), fullprof = numeric(),
                       prof = numeric(),
                       swaps = numeric(),
                       commis = numeric(),
                       numdeals = numeric(),
                       numdoldeals = numeric(), dohodnost = numeric(),
                       sharp = numeric(), numoftradeddays = numeric())
  for (optimid in optimids) {
    optimfile <- DefineFileById(optimid)
    restab <- CalculateTimeIntervalData(paste0(reportpath, "IterationsResult\\", optimfile), quotestab,
                                     tabswap, curspotname, curfutname)
    cumres <- rbind(cumres, AnalyzeSimResultsStocks(optimid, restab, bank))
  }
}
AntiCumSum <- function(vec) {
  c(vec[1], diff(vec))
}
AnalyzeSimResultsStocks <- function(optimid, restab, bank) {
  restab$fullprof <- AntiCumSum(restab$fullprof)
  restab$prof <- AntiCumSum(restab$prof)
  restab$commis <- AntiCumSum(restab$commis)
  restab$numdeals <- AntiCumSum(restab$numdeals)
  restab$numdol <- AntiCumSum(restab$numdol)
  restab$swaps <- AntiCumSum(restab$swaps)
  numdates <- length(unique(restab$Date))
  numdatesnodeals <-length(unique(restab$Date[numdeals <= 0]))
  data.frame(id = optimid, fullprof = sum(restab$fullprof),
             prof = sum(restab$prof),
             swaps = sum(restab$swaps),
             commis = sum(restab$commis),
             numdeals = sum(restab$numdeals),
             numdoldeals = sum(restab$numdoldeals),
             dohodnost = 100*250*mean(restab$fullprof)/(bank),
             sharp = mean(restab$fullprof)/sd(fullprof),
             numoftradeddays = numdates - numdatesnodeals
             )
}
CalculateTimeIntervalDataStocks <- function(filepath, quotestab, tabswap, curspotname, curfutname) {
  tab <- fread(filepath, select = c(1, 3, 4, 5, 6, 11))
  tab[, Time := fastPOSIXct(Time, tz = "GMT")]
  tab[, Date := as.Date(Time)]
  tab[, Price := as.numeric(gsub(",", ".", Price))]
  tab[, Price := as.numeric(gsub(",", ".", Commission))]
  quotesdol <- quotestab$dol
  quotesfut <- quotestab$fut
  dates <- unique(quotesdol$Date)
  dealsspot <- CalculatePosStocks(tab, curspotname)
  dealsfut <- CalculatePosStocks(tab, curfutname)
  pos <- 0
  restab <- data.frame(Date = Sys.Date(), fullprof = NA, prof = NA,
                       commis = NA,  swapprof = NA,
                       numdeals = NA, numdol = NA)
  for (adate in dates) {
    quotesdolred <- quotesdol[Date == adate, ]
    quotesfutred <- quotesfut[Date == adate, ]
    tabswapred <- tabswap[Date == adate, ]
    swapmodifier <- 0
    if (pos > 0) {
       swapmodifier <- -abs(pos)*tabswapred$Ask[1]
    } else {
       swapmodifier <- abs(pos)*tabswapred$Bid[1]
    }

    dealsspotred <- dealsspot[Date <= adate, ]
    dealsfutred <- dealsfut[Date <= adate, ]
    mod <- GetModByInstr(curspotname)

    if (nrow(dealsspotred) == 0) {
      profspot <- swapmodifier
      numspot <- 0
      commisspot <- 0
    } else {
      lastpos <- dealsspotred$Pos[nrow(dealsspotred)]
      profspot <- swapmodifier + dealsspotred$Rpl[nrow(dealsspotred)] +
        ifelse(lastpos > 0, abs(lastpos)*dealsspotred$Bid[1], -abs(lastpos)*dealsspotred$Ask[1])
      numspot <- nrow(dealsspotred)
      commisspot <- sum(dealsspotred$Commission)
    }
    if (nrow(dealsfutred) == 0) {
      proffut <- 0
      numfut <- 0
      commisfut <- 0
    } else {
      lastpos <- dealsfutred$Pos[nrow(dealsfutred)]
      proffut <- swapmodifier + dealsfutred$Rpl[nrow(dealsfutred)] +
        ifelse(lastpos > 0, abs(lastpos)*dealsfutred$Bid[1], -abs(lastpos)*dealsfutred$Ask[1])
      numfut <- nrow(dealsfutred)
      commisfut <- sum(dealsfutred$Commission)
    }

    restab <- rbind(restab, data.frame(
                Date = adate,
                fullprof = profspot*mod + proffut - commisspot - commisfut,
                prof = (profspot-swapmodifier)*mod + proffut,
                swaps = swapmodifier*mod,
                commis = commisspot + commisfut,
                numdeals = numspot + numfut,
                numdol = numspot
              ))
  }
restab
}


