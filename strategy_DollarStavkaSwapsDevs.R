#tab$newstavka, tab$newmodel --- mean level, tab$firstquant --- deviations
strategy_DSSD <- function(tab, params) {
  curpos <- 0
  pl <- 0
  numofdeals <- 0
  mydevs <- c()
  for (i in 1:nrow(tab)) {
#    print(tab[i,])
    if (tab$newstavka[i] > tab$newmodel[i] + tab$firstquant[i] & curpos > -params$maxpos) {
      curpos <- curpos - 1
      pl <- pl + trunc((tab$newmodel[i] + tab$firstquant[i])*1000/2.5)
      numofdeals <- numofdeals + 1
      mydevs <- c(mydevs, trunc(tab$firstquant[i]*1000/2.5))
    } else if (tab$newstavka[i] < tab$newmodel[i] - tab$firstquant[i] & curpos < params$maxpos) {
      curpos <- curpos + 1
      pl <- pl + trunc((- tab$newmodel[i] + tab$firstquant[i])*1000/2.5)
      numofdeals <- numofdeals + 1
      mydevs <- c(mydevs, trunc(tab$firstquant[i]*1000/2.5))
    } else if (params$closebymean) {
      if (curpos < 0 & tab$newstavka[i] < tab$newmodel[i]) {
        pl <- pl + trunc((- tab$newmodel[i]*abs(curpos))*1000/2.5)
        numofdeals <- numofdeals + abs(curpos)
        curpos <- 0
      } else if (curpos > 0 & tab$newstavka[i] > tab$newmodel[i]) {
        pl <- pl + trunc(tab$newmodel[i]*abs(curpos)*1000/2.5)
        numofdeals <- numofdeals + abs(curpos)
        curpos <- 0
      }
    }
  }
  if (curpos > 0) {
    pl <- pl + trunc(tab$newstavka[nrow(tab)]*abs(curpos)*1000/2.5)
    numofdeals <- numofdeals + abs(curpos)
  } else if (curpos < 0) {
    pl <- pl - trunc(tab$newstavka[nrow(tab)]*abs(curpos)*1000/2.5)
    numofdeals <- numofdeals + abs(curpos)
  }
list(pl = pl, numofdeals = numofdeals, curpos = curpos, devsize = summary(mydevs),
     numofdays = length(unique(tab$Date)))
}
