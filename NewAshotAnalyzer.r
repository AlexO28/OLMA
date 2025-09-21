library(data.table)
library(ggplot2)
library(MASS)

MyPipeLine <- function(aname) {
  deals <- GetSimpleDeals(paste0('D:\\AshotNewDeals\\Deals\\', aname, '.Deals.csv'))
  deals <- FillWithUnitDeals(deals)
  groups <- SplitInThreeGroups(deals)
  quotes <- GetQuotes(paste0('D:\\AshotNewDeals\\Bars\\', aname, '.csv'))
MatchIntoThreeGroups(groups, tabquotes)
}

AnalyzePipeLineResult <- function(tabs) {
  tab1 <- tabs[[1]]
  tab2 <- tabs[[2]]
  tab3 <- tabs[[3]]
  totnum <- nrow(tab1) + nrow(tab2) + nrow(tab3)
  num1 <- nrow(tab1)
  num2 <- nrow(tab2)
  num3 <- nrow(tab3)
  prof1 <- tab1[, sum(ifelse(Direction == 'Sell', Price1-Price2, Price2-Price1))]
  prof2 <- tab2[, sum(ifelse(Direction == 'Sell', Price1-Price2, Price2-Price1))]
  prof3 <- tab3[, sum(ifelse(Direction == 'Sell', Price1-Price2, Price2-Price1))]
  c(totnum, num1, num2, num3, prof1, prof2, prof3, prof1+prof2+prof3)
}

PlotBoxPlot <- function(tab) {
  tab[, Profit := ifelse(Direction == 'Sell', Price1-Price2, Price2-Price1)]
  #boxplot(Profit~Date, data = tab, col = "red")
  ggplot(data = tab, aes(Date, Profit)) + geom_boxplot(aes(group = Date))
  #truehist(tab$Profit)
}

PlotHist <- function(tab) {
  tab[, Profit := ifelse(Direction == 'Sell', Price1-Price2, Price2-Price1)]
  tab <- tab[, sum(Profit), by = Date]
  names(tab) <- c("Date", "Profit")
  #truehist(tab$Profit, nbins = 5, xlab = 'Profit')
  ggplot(data = tab, aes(Profit)) + geom_histogram(color = "red", fill = "red", bins=5)
}

GetSimpleDeals <- function(afile) {
  tab <- fread(afile, sep = ';', select = c(2, 3, 4, 5, 7), header = FALSE)
  names(tab) <- c("Time", "Volume", "Price", "Direction", "Comment")
  tab[, Date := substr(Time, 1, 10)]
  tab[, Date := as.Date(Date, format = "%d.%m.%Y")]
  tab <- tab[Date >= as.Date("2017-10-30"), ]
  tab[, Target := as.numeric(NA)]
  for (j in 1:nrow(tab)) {
    ahh <- tab$Comment[j]
	if (grepl('=', ahh)) {
	  posstart <- regexpr('=', ahh)
	  posend <- regexpr(',', ahh)
	  mypart <- substr(ahh, posstart+1, posend-1)
	  mypart <- gsub(' ', '', mypart)
	  mypart <- gsub(' ', '', mypart)
	  mypart <- gsub('В', '', mypart)
	  tab$Target[j] <- as.numeric(mypart)
	}
  }
 tab
}

FillWithUnitDeals <- function(tab) {
  res <- tab[1, ]
  lval <- 1
  for (j in 1:nrow(tab)) {
    #print(j)
	#print(lval)
    if (j > 1) {
	  res <- rbind(res, tab[j, ])
	  lval <- lval + 1
	}
	num <- res$Volume[lval]
	res$Volume[lval] <- 1
    if (num > 1) {
	  for (i in 1:(num-1)) {
	    res <- rbind(res, tab[j, ])
	    lval <- lval + 1
	    res$Volume[lval] <- 1 
	  }
	}	
  }
res
}

SplitInThreeGroups <- function(tab) {
  tabenter <- tab[!is.na(Target), ]
  tabexit <- tab[is.na(Target) & !grepl('Stop', Comment), ]
  tabstop <- tab[is.na(Target) & grepl('Stop', Comment), ]
list(tabenter, tabexit, tabstop)
}

GetQuotes <- function(afile) {
  tab <- fread(afile, sep = ';', select = c(1, 5))
  tab[, Date := as.Date(substr(Date, 1, 10), format = '%d.%m.%Y')]
  tab = tab[, tail(Close, 1), by = Date]
  names(tab) <- c("Date", "Close")
tab
}

MatchIntoThreeGroups <- function(groups, tabquotes) {
  tabenter <- as.data.table(as.data.frame(groups[[1]]))
  tabexit <- as.data.table(as.data.frame(groups[[2]]))
  tabstop <- as.data.table(as.data.frame(groups[[3]]))
    
  tab <- rbind(rbind(tabenter, tabexit), tabstop)
  dates <- unique(tab$Date)
  tabenter[, id := 1:nrow(tabenter)]
  tabexit[, id := 1:nrow(tabexit)]
  tabstop[, id := 1:nrow(tabstop)]
  group1info <- data.table(Price1 = numeric(), Price2 = numeric(), Direction = character(), Date = as.Date(numeric()))
  group2info <- data.table(Price1 = numeric(), Price2 = numeric(), Direction = character(), Quote = numeric(), Date = as.Date(numeric()))
  group3info <- data.table(Price1 = numeric(), Price2 = numeric(), Direction = character(), Date = as.Date(numeric()))

  for (adate in dates) {
    print(adate)
	tabenterred <- tabenter[Date == adate, ]
	tabexitred <- tabexit[Date == adate, ]
	tabstopred <- tabstop[Date == adate, ]
	tabexitred$matched <- FALSE
	tabenterred$matched <- FALSE
	tabstopred$matched <- FALSE
	for (j in 1:nrow(tabenterred)) {
	  target0 <- tabenterred$Target[j]
	  dir0 <- tabenterred$Direction[j]
	  #print(j)
	  if (dir0 == 'Sell') {
	    tabred <- tabexitred[(Price <= target0) & (matched == FALSE) & (Direction == 'Buy'), ]
		if (nrow(tabred)>0) {
		  tabenterred$matched[j] <- TRUE
		  astr <- tabred[1, ]
		  #print('1')
		  #print(astr)
		  group1info <- rbind(group1info, data.table(Price1 = tabenterred$Price[j], Price2 = astr$Price[1], Direction = dir0, Date = as.Date(adate)))
		  #print(group1info)
		  myid <- astr$id
		  tabexitred[id==myid, matched := TRUE]
		}
	  } else if (dir0 == 'Buy') {
	    tabred <- tabexitred[(Price >= target0) & (matched == FALSE) & (Direction == "Sell"), ]
		if (nrow(tabred)>0) {
		  tabenterred$matched[j] <- TRUE
		  astr <- tabred[1, ]
		  #print(2)
		  #print(astr)
		  group1info <- rbind(group1info, data.table(Price1 = tabenterred$Price[j], Price2 = astr$Price[1], Direction = dir0, Date = as.Date(adate)))
		  myid <- astr$id
		  tabexitred[id==myid, matched := TRUE]
		}
	  }
	}
	tabenterred2 <- tabenterred[matched == FALSE, ]
	#print(c(nrow(tabenterred), nrow(tabenterred2), nrow(tabstopred), nrow(tabexitred)))
	#stop()
	if (nrow(tabenterred2)>0) {
	  for (j in 1:nrow(tabenterred2)) {
	    dir0 <- tabenterred2$Direction[j]
		price0 <- tabenterred2$Price[j]
	    #print(j)
		if (dir0 == 'Sell') {
		  tabred <- tabstopred[(Price >= price0) & (matched == FALSE) & (Direction == 'Buy'), ]
		  if (nrow(tabred)>0) {
		    tabenterred2$matched[j] <- TRUE
			astr <- tabred[1, ]
			group2info <- rbind(group2info, data.table(Price1 = price0, Price2 = astr$Price[1], Direction = dir0, Quote = tabquotes[Date == adate, Close], Date = as.Date(adate)))
			myid <- astr$id
			tabstopred[id == myid, matched  := TRUE]
		  }
		} else if (dir0 == 'Buy') {
		  tabred <- tabstopred[(Price <= price0) & (matched == FALSE) & (Direction == 'Sell'), ]
		  if (nrow(tabred)>0) {
		    tabenterred2$matched[j] <- TRUE
			astr <- tabred[1, ]
			group2info <- rbind(group2info, data.table(Price1 = price0, Price2 = astr$Price[1], Direction = dir0, Quote = tabquotes[Date == adate, Close], Date = as.Date(adate)))
			myid <- astr$id
			tabstopred[id == myid, matched  := TRUE]
		  }		
		}
	  }
	}
	#print(c(dim(tabenterred2), dim(tabexitred), dim(tabstopred)))
	tabtofill <- rbind(tabenterred2[matched == FALSE,], rbind(tabexitred[matched == FALSE], tabstopred[matched == FALSE]))
	if (nrow(tabtofill)>0) {
	  for (j in 1:nrow(tabtofill)) {
	    group3info <- rbind(group3info, data.table(Price1 = tabtofill$Price[j], Price2 = tabquotes[Date == adate, Close], Direction = tabtofill$Direction[j], Date = as.Date(adate)))
	  }
	}
  }
  list(group1info, group2info, group3info)
}

