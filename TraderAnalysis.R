#requires DollarStavkaGraphs.R
#requires CalculatePlForShiny.R from Saule
GetDataForTraderAnalysis <- function(adate, portfolio1, portfolio2, timestart, timeend) {
  #Get Quotes
  tabquotes <- FillStavkaTab(adate, adate, futsec = "SIZ6@FORTS",
                             candletype = "1sVol50")
  #Get Deals
  dat <- GetBrandNewDeals(portfolio1, portfolio2, adate, adate, "USD000UTSTOM@CETS", "SIZ6@FORTS",
                               "", "", timestart, timeend)

  tabdoltom <- dat$tom
  tabdoltod <- dat$tod
  return(tabdoltom)
  tabfut <- dat$fut
  if (nrow(tabdoltod) > 0) {
    swapres <- NewAnalyzeTodTomDeals(datestart, dat$tod, dat$tom)
    tabdol <- swapres$restab
#    swapres <- list(posstart = swapres$swappos, swapprofit = swapres$swapprof)
  } else {
    tabdol <- tabdoltom
#    swapres <- list(posstart = 0,swapprofit = 0)
  }
list(quotes = tabquotes, dol = tabdol, fut = tabfut)
}
