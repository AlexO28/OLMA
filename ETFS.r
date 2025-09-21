library(data.table)

GetETFData <- function(storagepath, afile) {
  tab <- fread(paste0(storagepath, afile))
  tab[, dt := as.POSIXct(Date, format = '%Y%m%d %H:%M:%S')]
  tab[, Date := as.Date(dt)]
}
GetBuyHoldProfs <- function(tab, control = 0) {
  tab <- tab[Date >= as.Date("2017-01-01"), ]
  tabgr <- tab[, list(First = head(Open, 1), Last = tail(Close, 1)), by = Date]
  if (control == 1) {
    tabgr$First <- tabgr$First[1]
    tabgr[, Prof := diff(c(0, Last - First))]
  } else {
    tabgr[, Prof := Last-First]
  }
  tabgr
}
GetStat <- function(tab) {
  sharp <- tab[, mean(Prof)/sd(Prof)]
  goal <- -0.0001
  cumgoal <- 0
  for (j in 1:nrow(tab)) {
    if (tab$Prof[j] >= 0) {
	  if (cumgoal < goal) {
	    goal <- cumgoal
		cumgoal <- 0
	  }
	} else {
	  cumgoal <- cumgoal + tab$Prof[j]
	}
  }
  if (cumgoal < goal) {
	 goal <- cumgoal
	 cumgoal <- 0
  }
  #print(cumgoal)
  #print(goal)
  go <- max(tab[, max(First)], tab[, max(Last)])*0.1
  drawdown <- abs(goal/go)
  #print(drawdown)
  #print(go)
  recoveryfactor <- tab[, sum(Prof)]/(go*drawdown)
list(sharp = sharp, drawdown = drawdown, recoveryfactor=recoveryfactor)
}
GetStatData  <- function(storagepath) {
  files <- list.files(storagepath)
  resframe <- data.frame(Ticker = character(length(files)),
                         sharp = numeric(length(files)),
						 drawdown = numeric(length(files)),
						 recovery = numeric(length(files)))
  count <- 1						 
  for (afile in files) {
    print(afile)
	print(count)
	tab <- GetETFData(storagepath, afile)
	tab <- GetBuyHoldProfs(tab)
	print(head(tab))
	mystat <- GetStat(tab)
	resframe$Ticker[count] <- afile
	resframe$sharp[count] <- mystat$sharp
	resframe$drawdown[count] <- mystat$drawdown
	resframe$recovery[count] <- mystat$recoveryfactor
	count <- count + 1
  }
resframe
}