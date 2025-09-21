library(fasttime)
library(boot)
CalcTotalPl <- function(tab) {
  tab[, totalprofit := profitplus + profitminus + profitbefore + profitafter]
  tab[, totalquant := quantityplus + quantityminus + quantitybefore + quantafter]
tab
}
GetFinResFromReport <- function(report) {
  tab <- fread(report, sep = ";", skip = 2, select = c(2, 11), col.names = c("Ticker", "Price"))
 tab
}
CalcBootstrapProfit <- function(entertab, exittab) {
  datestart <- as.Date("2017-07-01") 
  dateend <- as.Date("2017-08-08")
  entab <- as.data.table(as.data.frame(entertab))
  extab <- as.data.table(as.data.frame(exittab))
  entab[, Date := as.Date(Time)]
  extab[, Date := as.Date(Time)]
  entab <- entab[(Date >= datestart) & (Date <= dateend), ]
  extab <- extab[(Date >= datestart) & (Date <= dateend), ]
  extab <- extab[!(grepl("oss", Comment)), ]
  entab <- entab[JoinId %in% extab$id, ]
  extab <- extab[JoinId %in% entab$id, ]
  entab[, prof := ifelse(Direction == "Sell", Price, -Price)]
  extab[, prof := ifelse(Direction == "Sell", Price, -Price)]
  restab <- merge(entab, extab, by.x = "id", by.y = "JoinId")
  restab[, prof := prof.x + prof.y]
  return(restab)
  CalcByIds <- function(data, indices) {
    sum(data$prof[indices])
  }
  vec <- boot(data = restab, statistic = CalcByIds, R = 1000)$t
  c(quantile(vec, 0.05), quantile(vec, 0.95))
}

CalcAggStat2 <- function(entab, extab, finresfirst, finreslast, datestart = as.Date("2017-07-01"), dateend = as.Date("2017-08-08")) {
  entab <- as.data.table(as.data.frame(entab))
  extab <- as.data.table(as.data.frame(extab))
  entab[, Date := as.Date(Time)]
  extab[, Date := as.Date(Time)]
  entab <- entab[(Date >= datestart) & (Date <= dateend), ]
  extab <- extab[(Date >= datestart) & (Date <= dateend), ]
  files <- unique(c(extab$filename, entab$filename))
  restab <- data.frame(filename = character(),
                       quantityplus = numeric(),
					   profitplus = numeric(),
					   quantityminus = numeric(),
					   profitminus = numeric(),
					   posafter = numeric(),
					   quantafter = numeric(),
					   profitafter = numeric(),
					   quantitybefore = numeric(),
					   posbefore = numeric(),
					   profitbefore = numeric())
  for (afile in files) {
 # afile <- "CVX"
    entabred <- entab[filename == afile, ]
	extabred <- extab[filename == afile, ]
	extabredgood <- extabred[!(grepl("oss", Comment)), ]
	extabredbad <- extabred[(grepl("oss", Comment)), ]
	
	tab1 <- entabred[JoinId %in% extabredgood$id, ]
	tab2 <- extabredgood[JoinId %in% entabred$id, ]
#	print(nrow(extabred))
#	print(nrow(entabred))
#	print(nrow(tab1))
#	print(nrow(tab2))
    quantplus <- nrow(tab1)
    profplus <- tab1[, sum(ifelse(Direction == "Sell", Volume*Price, -Volume*Price))] + tab2[, sum(ifelse(Direction == "Sell", Volume*Price, -Volume*Price))]
# print(profplus)
#	return(list(tab1, tab2))
	
	tab1 <- entabred[JoinId %in% extabredbad$id, ]
	tab2 <- extabredbad[JoinId %in% entabred$id, ]
    quantminus <- nrow(tab1)
    profminus <- tab1[, sum(ifelse(Direction == "Sell", Volume*Price, -Volume*Price))] + tab2[, sum(ifelse(Direction == "Sell", Volume*Price, -Volume*Price))]

	tab1 <- entabred[!(JoinId %in% extabred$id), ]
	quantafter <- nrow(tab1)
	posafter <- tab1[, sum(ifelse(Direction == "Buy", Volume, -Volume))]
	if (abs(posafter)>0.00001) {
	  myprice <- finreslast[Ticker == afile, Price]
	  if (length(myprice) == 0) {
	    myprice <- NA
	  }
	  profafter <- tab1[, sum(ifelse(Direction == "Sell", Price - myprice, myprice - Price))]
	} else {
	  profafter <- 0
	}
	
	tab2 <- extabred[!(JoinId %in% entabred$id), ]
	quantbefore <- nrow(tab2)
	posbefore <- tab2[, sum(ifelse(Direction == "Buy", Volume, -Volume))]
	if (abs(posbefore)>0.00001) {
	  myprice <- finresfirst[Ticker == afile, Price]
	  if (length(myprice) == 0) {
	    myprice <- NA
	  }
	  profbefore <- tab1[, sum(ifelse(Direction == "Sell", Price - myprice, myprice - Price))]
	} else {
	  profbefore <- 0
	}
#	print(profafter)
#	print(c(afile, quantplus, profplus, quantminus, profminus, posafter, quantafter, profafter, quantbefore, posbefore, profbefore))
	restab <- rbind(restab, data.frame(filename = afile,
                       quantityplus = quantplus,
					   profitplus = profplus,
					   quantityminus = quantminus,
					   profitminus = profminus,
					   posafter = posafter,
					   quantafter = quantafter,
					   profitafter = profafter,
					   quantitybefore = quantbefore,
					   posbefore = posbefore,
					   profitbefore = profbefore))
  }
restab
}
CalcAggStatMod <- function(entab, extab, finresfirst, finreslast, datestart = as.Date("2017-07-01"), dateend = as.Date("2017-08-08")) {
  entab <- as.data.table(as.data.frame(entab))
  extab <- as.data.table(as.data.frame(extab))
  entab[, Date := as.Date(Time)]
  extab[, Date := as.Date(Time)]
  entab <- entab[(Date >= datestart) & (Date <= dateend), ]
  extab <- extab[(Date >= datestart) & (Date <= dateend), ]
  files <- unique(c(extab$filename, entab$filename))
  restab <- data.frame(filename = character(),
                       quantityplus = numeric(),
					   profitplus = numeric(),
					   quantityminus = numeric(),
					   profitminus = numeric(),
					   quantityexnoentr = numeric(),
					   posexnoentr = numeric(),
					   profitexnoentr = numeric(),
					   quantityentrnoexit = numeric(),
					   posentrnoexit = numeric(),
					   profitentrnoexit = numeric())
  for (afile in files) {
    afileshort <- substring(afile, 1, nchar(afile) - 10)
    goodtab <- extab[!is.na(JoinId) & (filename == afile), ]
    goodtab[, prof := as.numeric(NA)]
    goodtab[Direction == "Buy", prof := Volume*(TrueEnterPrice-Price)]
    goodtab[Direction == "Sell", prof := -Volume*(TrueEnterPrice-Price)]  
	goodtabplus <- goodtab[!(grepl("oss", Comment)),]
	goodtabminus <- goodtab[(grepl("oss", Comment)),]
    quantplus <- nrow(goodtabplus)
	quantminus <- nrow(goodtabminus)
	profplus <- goodtabplus[, sum(prof)]
	profminus <- goodtabminus[, sum(prof)]
	
	enbadtab <- entab[is.na(JoinId) & (filename == afile), ] #entrance without exit
	exbadtab <- extab[is.na(JoinId) & (filename == afile), ] #exit without enter

	if (nrow(exbadtab)>0) {
	  exbadtab[, Pos := ifelse(Direction == "Buy", Volume, -Volume)]
	  exsumpos <- exbadtab[, sum(Pos)]
	  exbadquant <- nrow(exbadtab)
	  if (abs(exsumpos) > 0.1) {
    	ticker <- strsplit(afileshort, "_")[[1]][1]
	    print(ticker)
	    myprice <- finresfirst$Price[finresfirst$Ticker == ticker]
		print(myprice)
		if (length(myprice) == 0) {
		  myprice <- NA
		}
        exbadtab[, TrueEnterPrice := myprice]
	    exbadtab[, prof := as.numeric(NA)]	
		exbadtab[Direction == "Buy", prof := Volume*(TrueEnterPrice-Price)]
        exbadtab[Direction == "Sell", prof := -Volume*(TrueEnterPrice-Price)]  
        exbadprof = exbadtab[, sum(prof)]
	  } else {
	    exbadprof <- 0
	  }
	} else {
	  exbadprof <- 0
	  exsumpos <- 0
	  exbadquant <- 0
	}
	
	if (nrow(enbadtab)>0) {
	  enbadtab[, Pos := ifelse(Direction == "Buy", Volume, -Volume)]
	  ensumpos <- enbadtab[, sum(Pos)]
	  enbadquant <- nrow(enbadtab)
	  if (abs(ensumpos) > 0.1) {
    	ticker <- strsplit(afileshort, "_")[[1]][1]
	    print(ticker)
	    myprice <- finreslast$Price[finreslast$Ticker == ticker]
		print(myprice)
		if (length(myprice) == 0) {
		  myprice <- NA
		}
        enbadtab[, TrueEnterPrice := myprice]
	    enbadtab[, prof := as.numeric(NA)]	
		enbadtab[Direction == "Buy", prof := Volume*(TrueEnterPrice-Price)]
        enbadtab[Direction == "Sell", prof := -Volume*(TrueEnterPrice-Price)]  
        enbadprof = enbadtab[, sum(prof)]
	  } else {
	    enbadprof <- 0
	  }
	} else {
	  enbadprof <- 0
	  ensumpos <- 0
	  enbadquant <- 0
	}

    restab <- rbind(restab, data.frame(
	               filename = afileshort,
				   quantityplus = quantplus,
				   profitplus = round(profplus, 2),
				   quantityminus = quantminus,
				   profitminus = round(profminus, 2),
				   quantityexnoentr = exbadquant,
				   posexnoentr = exsumpos,
				   profitexnoentr = round(exbadprof, 2),
				   quantityentrnoexit = enbadquant,
				   posentrnoexit = ensumpos,
				   profitentrnoexit = round(enbadprof, 2)
				   ))
  }
restab
}

CalcAggStat <- function(res0, control, finres) {
  finres <- as.data.frame(finres)
  res0[, Date := as.Date(Time)]
  if (control == 1) {
    res0 <- res0[(Date >= as.Date("2017-07-01")) & (Date <= as.Date("2017-07-25")), ]
  } else if (control == 2) {
    res0 <- res0[(Date < as.Date("2017-07-01")) & (Date >= as.Date("2017-06-01")), ]  
  }
  resold <- res0[!(is.na(TargetPrice) & !is.na(TrueEnterPrice)), ]
  res <- res0[is.na(TargetPrice) & !is.na(TrueEnterPrice), ]
  print("with exit")
  print(nrow(res))
  print("suspicious")
  print(nrow(resold))
  print("trial 1")
  print(nrow(resold[id %in% unique(res$EnterId)]))
  print(nrow(resold[is.na(ExitId), ]))
  print(nrow(resold[is.na(EnterId), ]))

  resold <- resold[is.na(ExitId), ]
  #res <- res[!is.na(Price) & !is.na(TrueEnterPrice), ]
  files <- unique(res$filename)
  res[, prof := 0]
  res[Direction == "Buy", prof := Volume*(TrueEnterPrice-Price)]
  res[Direction == "Sell", prof := -Volume*(TrueEnterPrice-Price)]  
  exfr <- data.frame(name = character(), quantgood = numeric(), quantbad = numeric(),
                     profgood = numeric(), profbad = numeric())
  for (afile in files) {
    afileshort <- substring(afile, 1, nchar(afile) - 10)
	resred <- res[filename == afile, ]
	resoldred <- resold[filename == afile, ]
	if (nrow(resold)>0) {
	  ticker <- strsplit(afileshort, "_")[[1]][1]
	  print(ticker)
	  myprice <- finres[finres[, 2] == ticker, 11]
	  #resoldred$TrueEnterPrice <- myprice
	  print(myprice)
	  next
      resoldred[, prof := 0]
      resoldred[Direction == "Buy", prof := Volume*(TrueEnterPrice-Price)]
      resoldred[Direction == "Sell", prof := -Volume*(TrueEnterPrice-Price)]
      num3 <- resoldred[, sum(prof)]	  
	} else {
	  num3 <- 0
	  next
	}
    res1 <- resred[(grepl("oss", Comment)), ]
    res2 <- resred[!(grepl("oss", Comment)), ]
	num1 <- res1[, sum(prof)]
	num2 <- res2[, sum(prof)]
	exfr <- rbind(exfr, data.frame(name = afileshort,
	                               quantgood = nrow(res2),
								   quantbad = nrow(res1),
								   quantother = nrow(resoldred),
								   profgood = num2,
								   profbad = num1,
								   profother = num3))
  }
 exfr
}
RepresentByUnitDeals <- function(deals) {
  resdeals <- deals[0, ]
  for (j in 1:nrow(deals)) {
    resdeals <- rbind(resdeals, do.call("rbind", replicate(deals$Volume[j], deals[j, ], simplify = FALSE)))
  }
  resdeals$Volume <- 1
return(resdeals)
}
MainScriptMod <- function(storagepath) {
  filelist <- list.files(storagepath)
  first <- TRUE
  startid <- 1
  for (afile in filelist) {
    print(afile)
	deals <- fread(paste(storagepath, afile, sep = "\\"), sep = ";", dec = ",")
	deals <- as.data.frame(deals)
	deals <- data.table(Time = fastPOSIXct(strptime(deals[, 2], format = "%d.%m.%Y %H:%M:%S")), 
	                    Volume = deals[, 3], Price = deals[, 4], Direction = deals[, 5],
	                    Comment = deals[, 7])
    deals <- na.omit(RepresentByUnitDeals(deals))
    deals$TargetPrice <- as.numeric(NA)
	deals$JoinId <- as.numeric(NA)
	deals[grepl("=", Comment), TargetPrice := ParseTargetPriceFromComment(Comment)]
	deals[, id := (startid):(nrow(deals)+startid-1)]
	startid <- (nrow(deals)+startid)
	deals[, filename := strsplit(afile, "_")[[1]][1]]

	entertab <- as.data.table(deals[!is.na(TargetPrice), ])
	exittab <- as.data.table(deals[is.na(TargetPrice), ])
	
    for (ids in entertab[, id]) {
	  entertime <- entertab[id == ids, Time]
	  enterdir <- entertab$Direction[entertab$id == ids]
	  targetprice <- entertab[id == ids, TargetPrice]
	  dealsred <- exittab[(Time >= entertime) & is.na(JoinId) & (Direction != enterdir), ]
	  matched <- FALSE
	  print(ids)
	  if (nrow(dealsred)>0) {
  	    for (j in 1:nrow(dealsred)) {
	  	  exitid <- dealsred$id[j]
	      if (grepl("oss", dealsred$Comment[j])) {
		    if (enterdir == "Sell") {
		      if (dealsred$Price[j] > targetprice) {
			    matched <- TRUE
			  }
		    } else if (enterdir == "Buy") {
		      if (dealsred$Price[j] < targetprice) {
			    matched <- TRUE
		  	  }
		    }
		  } else {
		    if (enterdir == "Sell") {
		      if (dealsred$Price[j] <= targetprice + 0.000001) {
			    matched <- TRUE
			  }
		    } else if (enterdir == "Buy") {
		      if (dealsred$Price[j] >= targetprice - 0.000001) {
			    matched <- TRUE
			  }
		    }
		  }
		  if (matched) {
		    entertab[id == ids, JoinId := exitid]
		    exittab[id == exitid, JoinId := ids]
		    break
		  }
	    }
	  }
	}
	if (first) {
	  first <- FALSE
	  resentertab <- entertab
	  resexittab <- exittab
	} else {
	  resentertab <- rbind(resentertab, entertab)
	  resexittab <- rbind(resexittab, exittab)
	}
  }
return(list(enter = resentertab, exit = resexittab))
}

MainScript <- function(checkval = "ALL", storagepath = "C:\\OLMA\\dealsigor\\Deals\\", storagepath2 = "C:\\OLMA\\dealsigor\\Bars\\", atype = "America") {
  filelist <- list.files(storagepath)
  thefirst <- TRUE
  for (afile in filelist) {
    print(afile)
	deals <- fread(paste(storagepath, afile, sep = "\\"), sep = ";", dec = ",")
	deals <- as.data.frame(deals)
	deals <- data.table(Time = fastPOSIXct(strptime(deals[, 2], format = "%d.%m.%Y %H:%M:%S")), 
	                    Volume = deals[, 3], Price = deals[, 4], Direction = deals[, 5],
	                    Comment = deals[, 7])
	deals$TargetPrice <- as.numeric(NA)
	deals$EnterId <- as.numeric(NA)
	deals$ExitId <- as.numeric(NA)
	deals[grepl("=", Comment), TargetPrice := ParseTargetPriceFromComment(Comment)]
	
	deals[, id := 1:nrow(deals)]
	deals[, Special := FALSE]
    deals[, EnterPrice := as.numeric(NA)]
    deals[, EnterTime := as.POSIXct(NA)]
	deals[, TrueEnterPrice := as.numeric(NA)]
	deals[, filename := afile]
	
	for (ids in deals[!is.na(TargetPrice), id]) {
	  #we take enter and search for exit
	  dealsred <- deals[(Direction != deals$Direction[ids]) & (abs(Price - deals$TargetPrice[ids])<0.000001) & (is.na(EnterId)), ]
	  if (nrow(dealsred)>0) {
	    id2 <- dealsred$id[1]
		deals$ExitId[ids] <- id2
		deals$EnterId[id2] <- ids 
		
		deals$EnterPrice[id2] <- deals$TargetPrice[ids]
		deals$EnterTime[id2] <- deals$Time[ids]
		deals$TrueEnterPrice[id2] <- deals$Price[ids]
	  }
	}
	
	for (ids in deals[is.na(EnterId) & is.na(TargetPrice), id]) {
	  #we take exit and search for enter
	  for (j in 1:(ids-1)) {
	    if (is.na(deals$ExitId[j]) & !is.na(deals$TargetPrice[j])) {
		  if (deals$Direction[j] != deals$Direction[ids]) {
		    deals$ExitId[j] <- ids
			deals$EnterId[ids] <- j
			deals$Special[ids] <- TRUE
			deals$EnterPrice[ids] <- deals$TargetPrice[j]
			deals$EnterTime[ids] <- deals$Time[j]
			deals$TrueEnterPrice[ids] <- deals$Price[j]
		  }
		}
	  }
	}
	
	deals[!grepl("stop loss", Comment), Special := FALSE]
	if (as.character(checkval) != "ALL") {
      deals <- deals[Special == checkval, ]
	}
	#deals <- deals[is.na(TargetPrice), ]
	print(deals)
	if (nrow(deals) > 0) {
     # filename <- tolower(paste0(substr(afile, 1, 3), substr(afile, 5, 7)))
	 # print(filename)
	 # periodname <- gsub(".*[_]([^.]+)[.].*", "\\1", afile)
	 # print(periodname)

    if (thefirst) {
      thefirst <- FALSE
	  restab <- deals
    }	else {
      restab <- rbind(restab, deals)
    }	 
	next
	
	  if (atype == "America") {
  	    filename <- (strsplit(afile, "\\."))[[1]][1]
	    fileshort <- (strsplit(filename, "_"))[[1]][1]
	    periodname <- (strsplit(filename, "_"))[[1]][2]
	  } else {
	    strlist <- (strsplit(afile, "\\."))[[1]]
		filename <- paste(strlist[1], strlist[2], sep = ".")
		periodname <- (strsplit(strlist[2], "_"))[[1]][2]
		fileshort <- (strsplit(afile, "_"))[[1]][1]
	  }
	  tabquotes <- fread(paste0(storagepath2, "//", filename, ".csv"), dec = ",")
	  tabquotes <- as.data.frame(tabquotes)
	  tabquotes <- data.table(Time = fastPOSIXct(strptime(tabquotes[, 1], format = "%d.%m.%Y %H:%M:%S")), 
	                          Low = tabquotes[, 4], High = tabquotes[, 3])
	  print(head(tabquotes))

      deals$newprice <- as.numeric(NA)
	  deals$newtime <- as.POSIXct(NA)
	  deals$check <- FALSE
	  deals$lastprice <- as.numeric(NA)
	  
	  for (j in 1:nrow(deals)) {
	    timecrit <- tabquotes[Time <= deals$Time[j], max(Time)]
		print(paste("timecrit", timecrit))
	    if (deals$Direction[j] == "Buy") {
		  #stoploss deal was a buy-deal
		  tabquotesred <- tabquotes[Time >= timecrit & High <= deals$EnterPrice[j], ]
		  if (nrow(tabquotesred) > 0) {
		    deals$newprice[j] <- deals$EnterPrice[j]
			deals$newtime[j] <- tabquotesred$Time[1]
		  } else {
		    deals$newprice[j] <- tabquotes$High[nrow(tabquotes)]
		    deals$newtime[j] <- tabquotes$Time[nrow(tabquotes)]
			deals$check[j] <- TRUE
		  }
		  deals$lastprice[j] <- tabquotes$High[nrow(tabquotes)]
		} else if (deals$Direction[j] == "Sell") {
		  #stoploss deal was a sell-deal
		  tabquotesred <- tabquotes[Time >= timecrit & Low >= deals$EnterPrice[j], ]
		  if (nrow(tabquotesred) > 0) {
		  	deals$newprice[j] <- deals$EnterPrice[j]
			deals$newtime[j] <- tabquotesred$Time[1]
		  } else {
		    deals$newprice[j] <- tabquotes$Low[nrow(tabquotes)]
		    deals$newtime[j] <- tabquotes$Time[nrow(tabquotes)]
			deals$check[j] <- TRUE
		  }		  
		  deals$lastprice[j] <- tabquotes$Low[nrow(tabquotes)]
		}
	  }
	  
	deals$security <- fileshort
	deals$periodname <- periodname
	#deals$Comment <- NULL
	deals$TargetPrice <- NULL
	deals$EnterId <- NULL
	deals$ExitId <- NULL
	deals$id <- NULL
	deals$Special <- NULL 

	deals$diff <- as.numeric(NA)
    deals[Direction == "Buy", diff := -Price+newprice]
	deals[Direction == "Sell", diff := Price-newprice]
	deals$realprof <- as.numeric(NA)
	deals$pureprof <- as.numeric(NA)
	deals$loss <- as.numeric(NA)
	deals$realprofnew <- as.numeric(NA)
	deals$lossnew <- as.numeric(NA)
	
	deals[Direction == "Buy", realprof := Volume*(-Price + TrueEnterPrice)]
	deals[Direction == "Sell", realprof := Volume*(Price - TrueEnterPrice)]
	deals[Direction == "Buy", loss := Volume*(-Price + EnterPrice)]
	deals[Direction == "Sell", loss := Volume*(Price - EnterPrice)]
    deals[Direction == "Buy", pureprof := Volume*(TrueEnterPrice - EnterPrice)]
    deals[Direction == "Sell", pureprof := Volume*(EnterPrice - TrueEnterPrice)]	
	deals[Direction == "Buy", realprofnew := Volume*(-newprice + TrueEnterPrice)]
	deals[Direction == "Sell", realprofnew := Volume*(newprice - TrueEnterPrice)]
	deals[Direction == "Buy", lossnew := Volume*(-newprice + EnterPrice)]
	deals[Direction == "Sell", lossnew := Volume*(newprice - EnterPrice)]
	
    if (thefirst) {
      thefirst <- FALSE
	  restab <- deals
    }	else {
      restab <- rbind(restab, deals)
    }
	
#	print(deals)
#	stop()
    }
  }
return(restab)
  restab[, diffenter := difftime(Time, EnterTime, units = "days")]
  restab[, newdiff := difftime(newtime, EnterTime, units = "days")]
restab
}

ParseTargetPriceFromComment <- function(mycomment) {
  as.numeric(gsub(",", ".", gsub(".*[= ]([^|]+)[|].*", "\\1", mycomment)))
}

library(RODBC)
library(data.table)
options(stringsAsFactors = FALSE)
library(tm)

PrepareNamesFromDf <- function(mycharvec, mystopwords) {
  vec <- tolower(mycharvec)
  vec <- removeWords(vec, mystopwords)
  vec <- stripWhitespace(vec)
  vec <- gsub("[[:punct:]]", "", vec)
  strsplit(vec, " ")
}

GetDataFromExcel <- function(afile = "C:\\OLMA\\Data.xlsx") {
  con <- odbcConnectExcel(afile)
  tab <-  sqlFetch(con, "Лист1$")
  close(con)
  tab <- tab[, 1:2]
  names(tab) <- c("Name", "Text")
  tab <- as.data.table(tab)
  tab[, id := 1:nrow(tab)]
  tab <- tab[2:nrow(tab), ]
  tab2 <- na.omit(tab)
  tab2[, idnext := c(id[2:(length(id))], nrow(tab))]
  setkey(tab2, "id")
  setkey(tab, "id")  
  print(head(tab, 10))
  print(head(tab2, 10))
  tab3 <- tab[tab2, nomatch = 0]
  tab3 <- tab3[idnext - id == 1, ]
 tab3[, Date := GetDateFromText(Text)]
}

GetRawDataFromExcel <- function(afile = "C:\\OLMA\\DataNew.xlsx") {
  con <- odbcConnectExcel(afile)
  tab <-  sqlFetch(con, "Лист1$")
  close(con)
  tab <- tab[, 1:3]
  names(tab) <- c("Name", "Text", "ShortName")
  tab <- as.data.table(tab)
  tab[, id := 1:nrow(tab)]
tab
}

GetExdividendAndGatheringDatesFromExcel <- function(tab, keystr1, keystr2, keystr3) {
  #print(head(tab, 100))
  #first we deal with the gatherings
  tabgath <- tab[grepl(keystr1, Text), ]
  tabgath[, Date := GetDateFromText(Text)]
  tabdiv1 <- tab[grepl(keystr2, Name), ]
  tabdiv2 <- tab[grepl(keystr3, Text), ]
  tabdiv2 <- tabdiv2[1:nrow(tabdiv1), ]
  tabdiv2[, Date := GetDateFromText2(Text)]
  names(tabdiv2) <- c("del1", "del2", "del3", "del4", "Date")
  tabdiv <- cbind(tabdiv1, tabdiv2[1:nrow(tabdiv1), ])
  tabdiv <- tabdiv[, list(Name, Text, Date, ShortName)]
  tabgath <- tabgath[, list(Name, Text, Date, ShortName)]
rbind(tabgath, tabdiv)
}

ModernMatchNamesAndTickers <- function(tab, tickers) {
  for (j in 1:nrow(tickers)) {
	tab[grepl(tickers$Name[j], Name), ShortName := tickers$ShortName[j]]
  }
tab
}

ModernManualFix <- function(tab) {
  tab[grepl("CME", Name), ShortName := "CME"]
  tab[grepl("Alcoa", Name), ShortName := "AA"]
  tab[grepl("Google", Name), ShortName := "GOOG"]
  tab[grepl("Delta Air", Name), ShortName := "DAL"]
  tab[grepl("GILEAD", Name), ShortName := "GILD"]
  tab[grepl("Walgreen", Name), ShortName := "WBA"]
  tab[grepl("DU PONT", Name), ShortName := "DD"]
  tab[grepl("Alphabet", Name), ShortName := "GOOG"]
  tab[grepl("Bed Bath", Name), ShortName := "BBBY"]
  tab[grepl("DaVita", Name), ShortName := "DVA"]
  tab[grepl("Allergan", Name), ShortName := "AGN"]
  tab[grepl("International Business", Name), ShortName := "IBM"]
  tab[grepl("ConAgra", Name), ShortName := "CAG"]
  tab[grepl("Newell", Name), ShortName := "NWL"]
tab
}

GetDateFromText <- function(atext) {
  as.Date(substr(atext, 1, 10), format = "%d.%m.%Y")
}

GetDateFromText2 <- function(atext) {
  as.Date(substr(atext, nchar(atext)-10, nchar(atext)), format = "%d.%m.%Y")
}


VectComp <- function(strval, strlist) {
  strval %in% strlist[[1]]
}

VectCompVec <- function(strval, vec) {
  if (length(vec) == 0) {return(FALSE)}
  res <- logical(length(vec))
  for (j in 1:length(vec)) {
    res[j] <- strval %in% vec[[j]]
  }
  return(res)
}

MatchTickerAndInfo <- function(infostr, tickers, mystopwords) {
  tickers <- as.data.table(as.data.frame(tickers))
  names(tickers) <- c("ticker", "name")
  tickersinfo <- PrepareNamesFromDf(tickers$name, tolower(mystopwords))
  tickers$newtext <- tickersinfo
  count <- 0
  
  tickersvec <- character(length(infostr))
  errortab <- list()
  errcount <- 1
  
  for (j in 1:length(infostr)) {
    astr <- infostr[[j]]
	tickers2 <- as.data.table(as.data.frame(tickers))
	astr <- tolower(astr)
	johnsoncheck <- (astr == "johnson")
	if (sum(as.numeric(johnsoncheck)) >= 2) {
	  tickersvec[j] <- "JNJ"
	  next
	}
	#print(astr)
	#stop()
	for (i in 1:length(astr)) {
	  aword <- astr[i]
	  if (nchar(aword)>0) {
		tickers2 <- tickers2[VectCompVec(aword, tickers2$newtext), ]
	  }
	}
	if (nrow(tickers2) == 0) {
	  print(j)
	  print(astr)
	  print(tickers2)
	  print("No data")
	  errortab[[errcount]] <- astr
	  errcount <- errcount + 1
	  tickersvec[j] <- NA
	}
	if (nrow(tickers2)>1) {
	  print(j)
	  print(astr)
	  print(tickers2)
	  print("ambiguous data")
	  stop()
	  errortab <- rbind(errortab, astr)
	  tickersvec[j] <- NA
	}
	if (nrow(tickers2) == 1) {
	  count <- count + 1
	  tickersvec[j] <- tickers2$ticker[1]
	}
	#break
  }
list(tickers = tickersvec, errors = errortab)
}

ManualFix <- function(hdat, aname) {
  ids <- 1:length(hdat)
  ids[VectCompVec(aname, hdat)]
}

KillEventsInTheData <- function(storagepath, df, simple = TRUE) {
  filelist <- list.files(storagepath)
  for (j in 1:nrow(df)) {
    ticker <- df$ticker[j]
	adate <- as.Date(df$Date[j])
	if (!simple) {
	  afile <- filelist[grepl(paste0("^", ticker, "_"), filelist)]
	} else {
	  afile <- filelist[grepl(paste0("^", ticker, "\\."), filelist)]
	}
	if (length(afile) == 0) {
	  print(paste("no data", ticker))
	} else {
	  print(paste(afile, adate))
	  dat <- fread(paste(storagepath, afile, sep = "\\"), sep = ";")
	  dat[, mydate := as.Date(as.character(DATE), format = "%Y%m%d")]
	  dimprev <- nrow(dat)
	  dat <- dat[(mydate < adate-3) | (mydate > adate + 3), ]
	  dimcur <- nrow(dat)
	  if (dimcur != dimprev) {
	    print(c(dimcur, dimprev))
	    print("writing data")
		dat$mydate <- NULL
		antistoragepath <- paste(storagepath, "withoutevents", sep = "_")
		write.table(dat, paste(antistoragepath, afile, sep = "\\"), sep = ";", row.names = FALSE, quote = FALSE)
	  }
	}
  }
}
MakeTriplePlot <- function(quotesfile, deals1file, deals2file, fullmode = TRUE) {
  tabquotes <- fread(quotesfile, sep = ";", dec = ",")
  deals1 <- na.omit(fread(deals1file, sep = ";", dec = ","))
  deals2 <- fread(deals2file, sep = ";", dec = ",")
 #  print(head(tabquotes))
 # print(head(deals1))
#  print(head(deals2))
  
  tabquotes[, Date := fastPOSIXct(strptime(Date, format = "%d.%m.%Y %H:%M:%S"))]
  deals1[, Date := fastPOSIXct(strptime(Date, format = "%d.%m.%Y %H:%M"))]
#  deals2[, Date := fastPOSIXct(paste(Year, "-", Mounth, "-", Date, " ", Hours, ":", Minutes, ":", Seconds))]
  deals2[, Date := fastPOSIXct(strptime(paste(Date, Time), format = "%d.%m.%Y %H:%M:%S"))]
  deals2[, Date := Date + 3600*7]
  
  deals1[, Price := as.numeric(Price)]
  deals2[, TradePrice := as.numeric(TradePrice)]  
  tabquotes <- tabquotes[Date >= fastPOSIXct("2017-04-03 00:00:00") & Date <= fastPOSIXct("2017-04-10 00:00:00"), ] 
  
  
  print(unique(as.Date(tabquotes$Date)))
  print(summary(tabquotes))
  print(summary(deals1))
  print(summary(deals2))  
  
  yrange <- c(0.99*min(tabquotes$Close, deals1$Price, deals2$TradePrice), 1.01*max(tabquotes$Close, deals1$Price, deals2$TradePrice))
  print(yrange)
  if (fullmode) {
   plot(tabquotes$Date, tabquotes$Close, ylim = yrange)  
  } else {
    #tabquotes <- tabquotes[Date >= fastPOSIXct("2017-04-24 00:00:00"), ]
    #deals1 <- deals1[Date >= fastPOSIXct("2017-04-24 00:00:00"), ]
    #deals2 <- deals2[Date >= fastPOSIXct("2017-04-24 00:00:00"), ]
#	tabquotes <- tabquotes[Date <= tabquotes$Date[1] + 7*86400, ]
 #   deals1 <- deals1[Date <= tabquotes$Date[1] + 7*86400, ]
  #  deals2 <- deals2[Date <= tabquotes$Date[1] + 7*86400, ]
    plot(tabquotes$Date, tabquotes$Close, ylim = yrange, xaxt = "n")
    axis.POSIXct(1, at = unique(as.Date(tabquotes$Date)), labels = format(unique(as.Date(tabquotes$Date)), format = "%d.%m.%Y"))
  }
  #c(as.Date("2017-04-24"), as.Date("2017-04-29"))
  #axis.Date(1, at = as.Date("2017-04-25"), labels = TRUE, tcl = -0.3)
#  axis(1, xaxp = c(as.Date("2017-04-25"), as.Date("2017-04-29"), 86400))
  for (j in 1:nrow(deals1)) {
    acode <- ifelse(deals1$Position[j] == "Long", 24, 25)
	points(deals1$Date[j], deals1$Price[j], pch = acode, col = "green", bg = "green")
  }
  for (j in 1:nrow(deals2)) {
    acode <- ifelse(deals2$Direction[j] == "buy", 24, 25)
	points(deals2$Date[j], deals2$TradePrice[j], pch = acode, col = "red", bg = "red")
  }
  legend("topright", lty = 1, legend = c("Wealth Lab", "Kharevsky"),  col = c("green", "red"))
 list(tabquotes = tabquotes, deals1 = deals1, deals2 = deals2)
}

