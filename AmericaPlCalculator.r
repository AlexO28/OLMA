library(data.table)

GetClosingQuotes <- function(storagepath, adate) {
  afile <- paste(as.character(adate, format = "%d.%m.%Y"), "csv", sep = ".")
  print(afile)
  tab <- fread(paste(storagepath, afile, sep = "\\"), sep = ";", select = c(1, 2, 3, 5, 7, 44))
  tab
}

GetKolyaTrades <- function(storagepath, afile, afileswaps) {
  tab <- fread(paste(storagepath, afile, sep = "\\"), sep = ";", select = c(2, 4, 5, 6, 7, 12), dec = ",")
  tab[, TradeTime := as.POSIXct(TradeTime, format = "%d-%m-%Y %H:%M:%S")]
  tab[, Date := as.Date(TradeTime)]
  tabswaps <- fread(paste(storagepath, afileswaps, sep = "\\"), sep = ";", select = c(2, 7, 8, 10, 11, 14, 16, 19))
  tabswaps$Date <- as.Date(strptime(tabswaps$Date, format = "%d.%m.%Y"))
  list(tab = tab, swaps = tabswaps)
}
CalculateSwapProfit <- function(tabswaps, adate) {
  tabswaps <- as.data.table(as.data.frame(tabswaps))
  chars <- unique(tabswaps$Direction) 
  chars <- chars[order(chars)]
  print(chars)
  tabswaps <- tabswaps[Date <= adate, ]
  tabswaps <- tabswaps[!grepl("/", Direction), ]
  pureval <- tabswaps[Direction == chars[3], sum(Price*Volume)] + tabswaps[Direction == chars[2], -sum(Price*Volume)]
  totval <- pureval - tabswaps[, sum(Commission1 + Commission2)]
  list(pureval = pureval, totval = totval)
}
CalculateDealsProfit <- function(tab, storagepath, adate) {
  tabquotes <- GetClosingQuotes(storagepath, adate)
  names(tabquotes) <- c("ticker",  "fullname", "addressflag", "regime", "cathegory", "close")
  print(tabquotes[1, ])
  print(summary(tabquotes))
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[Date <= adate, ]
  securs <- unique(tab$Bars)
  fullpl <- 0
  for (secur in securs) {
    securshort <- strsplit(secur, "@")[[1]][1]
	print(securshort)
	tabred <- tab[Bars == secur, ]
	pl1 <- tabred[Direction == "Sell", sum(Price*Volume)] + tabred[Direction == "Buy", -sum(Price*Volume)]
	pos <- tabred[Direction == "Sell", sum(-Volume)] + tabred[Direction == "Buy", sum(Volume)]
	#print(c(pos, pl1))
	fullpl <- fullpl + pl1
	if (pos != 0) {
	  tabquotesred <- tabquotes[ticker == securshort & close > 0 & regime == "Режим основных торгов", ]
	  if (nrow(tabquotesred)>1) {
	    print(tabquotesred)
		stop("Inambiguity! Check algorithm!")
	  }
	  if (nrow(tabquotesred) == 0) {
	    return(NA)
	  }
	  closeprice <- tabquotesred$close[1]
	  fullpl <- fullpl + pos*closeprice
	}
    print(fullpl)	
#    break	
  }
fullpl
}
CalculateFullPl <- function(tab, tabswaps, storagepath, adate) {
  swapsinfo <- CalculateSwapProfit(tabswaps, adate)
  fullpl <- CalculateDealsProfit(tab, storagepath, adate)
 fullpl + swapsinfo$pureval
}
GetInfoForPlGraph <- function(tab, tabswaps, storagepath) {
  setkey(tab, TradeTime)
  dates <- unique(c(tab$Date, tabswaps$Date))
  dates <- dates[order(dates)]
  plinfo <- numeric(length(dates))
  for (j in 1:length(dates)) {
    adate <- dates[j]
    print(adate)
	plinfo[j] <- CalculateFullPl(tab, tabswaps, storagepath, adate)
  }
 list(dates = dates, pl = plinfo)
}

DrawBokehPlot <- function(res) {
  figure() %>%
    ly_lines(res$dates, res$pl, hover = c(res$dates, res$pl), color = "red", width = 2) %>%
	x_axis(label = "Date", format = list(days="%d.%m.%Y")) %>%
	y_axis(label = "pl")
} 