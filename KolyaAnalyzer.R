library(data.table)

GetGroupedNickData <- function(aname) {
  tab <- fread(aname, header = FALSE)
  names(tab) <- c("Ticker", "DateStart", "Profit", "DateEnd", "Go")
  tab[, Year := as.POSIXlt(DateStart)$year + 1900]
  tabgr <- tab[, list(meanprof = sum(Profit), numdeals = length(Profit), go = max(Go)), by = list(Year, Ticker)]
  tabgr[, cash := meanprof/go]
  tabgr
}

MainGetNickData <- function(aname) {
  tab <- GetGroupedNickData(aname)
  return(list(v1=tab[Year == 2015, ], v2=tab[Year == 2016, ], v3=tab[Year == 2017, ]))
}

CalcMetricBetweenTwoSets <- function (tab1, tab2, q1, q2, pow) {
  tab1 <- tab1[(meanprof > q1) & (numdeals > q2)]
  tab2red <- tab2[(meanprof > q1) & (numdeals > q2)]
  set1 <- tab1$Ticker
  set2 <- tab2red$Ticker
  setall <- length(union(set1, set2)) + 0.0001
  setinter <- length(intersect(set1, set2)) + 0.0001
  prof <- tab2[tab2$Ticker %in% tab1$Ticker, sum(meanprof)/sum(go)]
  if (is.na(prof)) {return(0)}
  if (prof>0) {
    return(prof*(setinter/setall)^pow)
  } else if (prof < 0) {
    return(prof*(setall/setinter)^pow)
  } else {
    return(0)
  }
}

MainKolyaOptimizer <- function(tab1, tab2, pow) {
  vec <- seq(0, 1, 0.05)
  res <- expand.grid(vec, vec)
  names(res) <- c("prof", "deals")
  res$val <- 0
  res$v1 <- 0
  res$v2 <- 0 
  res <- as.data.table(res)
  print(names(res))
  for (i in vec) {
    for (j in vec) {
	  tab <- rbind(tab1, tab2)
	  q1 <- tab[, quantile(cash, i)]
	  q2 <- tab[, quantile(numdeals, j)]
	  print(c(q1, q2))
      res[(prof == i) & (deals == j), val := CalcMetricBetweenTwoSets(tab1, tab2, q1, q2, pow)]
	  res[(prof == i) & (deals == j), v1 := q1]
	  res[(prof == i) & (deals == j), v2 := q2]
	}
  }
  setkey(res, "val")
res
}

MainKolyaChecker <- function(tab1, tab2, q1, q2) {
  tab1 <- tab1[(meanprof > q1) & (numdeals > q2)]
  tab2 <- tab2[(meanprof > q1) & (numdeals > q2)]
  set1 <- tab1$Ticker
  set2 <- tab2$Ticker
  setall <- length(union(set1, set2)) + 0.0001
  setinter <- length(intersect(set1, set2)) + 0.0001
  prof <- tab2[, sum(meanprof)/sum(go)]
return(c(prof, length(set1), length(set2), setinter, setall))
}