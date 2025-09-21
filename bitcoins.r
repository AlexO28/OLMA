PreprocessData <- function(tab) {
  GetType <- function(astr) {
    substr(astr, nchar(astr), nchar(astr))
  }
  GetExpir <- function(astr) {
    substr(astr, 5, 11)
  }
  tab <- as.data.table(as.data.frame(tab))
  tab[, opttype := GetType(V1)]
  tab[, Volume := V4]
  tab[, mydt := as.POSIXct(V5, format = '%m-%d %H:%M:%S')]
  tab[, expiry := GetExpir(V1)]
  tab[, mydate := as.Date(mydt)]
  tab[, myhour := as.POSIXlt(mydt)$hour]
tab
}

MakeFactorBoxPlots <- function(tab, control = 1) {
  expirys <- unique(tab$expiry)
  opttypes <- unique(tab$opttype)
  par(mfrow = c(2, 2))
  for (opttyp in opttypes) {
    for (expir in expirys) {
	  tabred <- na.omit(tab[(opttype == opttyp) & (expiry == expir), ])
	  if (control == 1) {
	    plot(tabred$mydate, tabred$V1, ylim = c(min(tabred$V1)*0.9, max(tabred$V1)*1.1), xlab = 'Date', ylab = 'Number of trades')
	  } else if (control == 2){
	    plot(tabred$mydate, tabred$V2, ylim = c(min(tabred$V2)*0.9, max(tabred$V2)*1.1), xlab = 'Date', ylab = 'Sum of volumes')	  
	  } else if (control == 3) {
	    plot(tabred$myhour, tabred$V1, ylim = c(min(tabred$V1)*0.9, max(tabred$V1)*1.1), xlab = 'Hour', ylab = 'Number of trades')	  
	  } else if (control == 4) {
	    plot(tabred$myhour, tabred$V2, ylim = c(min(tabred$V2)*0.9, max(tabred$V2)*1.1), xlab = 'Hour', ylab = 'Sum of volumes')	  	  
	  }
	  title(paste('Contract type', opttyp, 'with expiry', expir))
	}
  }
}

MakeSeveralHourPlots <- function(tab, control = 1) {
  tab[, mygroup := 0]
  tab[(myhour >=0) & (myhour <= 3), mygroup := 1]
  tab[(myhour >=4) & (myhour <= 7), mygroup := 2]  
  tab[(myhour >=8) & (myhour <= 11), mygroup := 3]  
  tab[(myhour >=12) & (myhour <= 15), mygroup := 4]  
  tab[(myhour >=16) & (myhour <= 19), mygroup := 5]  
  tab[(myhour >=18) & (myhour <= 23), mygroup := 6]  
  print(tab[mygroup == 0, ])
  par(mfrow = c(2, 3))
  for (group in 1:6) {
    tabred <- tab[mygroup == group, ]
	tabred <- tabred[, list(length(id), sum(Volume)), by = mydate]
	print(head(tabred))
	if (control == 1) {
	  plot(tabred$mydate, tabred$V1, ylim = c(min(tabred$V1)*0.9, max(tabred$V1)*1.1), xlab = 'Date', ylab = 'Number of trades')
	  abline(h = mean(tabred$V1), col = 'red')
	  print(c(group, mean(tabred$V1)))
	} else if (control == 2){
	  plot(tabred$mydate, tabred$V2, ylim = c(min(tabred$V2)*0.9, max(tabred$V2)*1.1), xlab = 'Date', ylab = 'Sum of volumes')
      abline(h = mean(tabred$V2), col = 'red')	  
	  print(c(group, mean(tabred$V2)))
	}
	if (group == 1) {
	  title(paste('Time between', 0, 'and', 3, 'hours'))
	} else 	if (group == 2) {
	  title(paste('Time between', 4, 'and', 7, 'hours'))
	} else 	if (group == 3) {
	  title(paste('Time between', 8, 'and', 11, 'hours'))
	} else 	if (group == 4) {
	  title(paste('Time between', 12, 'and', 15, 'hours'))
	} else 	if (group == 5) {
	  title(paste('Time between', 16, 'and', 19, 'hours'))
	} else 	if (group == 6) {
	  title(paste('Time between', 20, 'and', 23, 'hours'))
	}	
  }
}

AnalyzePastAndPresent <- function(tab, control) {
  tab[, mygroup := 0]
  tab[(myhour >=0) & (myhour <= 3), mygroup := 1]
  tab[(myhour >=4) & (myhour <= 7), mygroup := 2]  
  tab[(myhour >=8) & (myhour <= 11), mygroup := 3]  
  tab[(myhour >=12) & (myhour <= 15), mygroup := 4]  
  tab[(myhour >=16) & (myhour <= 19), mygroup := 5]  
  tab[(myhour >=20) & (myhour <= 23), mygroup := 6]  
  tab1 <- tab[mydate < as.Date("2017-11-01"), ]
  tab2 <- tab[mydate >= as.Date("2017-11-01"), ]
  htab1 <- tab1[, list(length(id), sum(Volume)), by = list(mygroup, mydate)]
  htab2 <- tab2[, list(length(id), sum(Volume)), by = list(mygroup, mydate)]
  if (control == 0) {
    return(rbind(htab1, htab2))
  }
  if (control == 1) {
    htab1 <- htab1[, list(mean(V1), mean(V2)), by = list(mygroup)]
    htab2 <- htab2[, list(mean(V1), mean(V2)), by = list(mygroup)]
  } else if (control == 2) {
    print(median(htab1$V1))
    htab1 <- htab1[, list(1.*median(V1), 1.*median(V2)), by = list(mygroup)]
    htab2 <- htab2[, list(1.*median(V1), 1.*median(V2)), by = list(mygroup)]  
  }
  par(mfrow = c(1, 2))
  plot(htab2$V1/htab1$V1, xlab = 'Group', ylab = 'Ratio')
  title(paste('Ratio of numbers of trades'))
  abline(h = mean(htab2$V1/htab1$V1), col = "red")
  plot(htab2$V2/htab1$V2, xlab = 'Group', ylab = 'Ratio')
  title(paste('Ratio of sums of volumes'))
  abline(h = mean(htab2$V2/htab1$V2), col = "red")
}