library(data.table)

GetFundData <- function(filename = 'D:\\RTSI\\RSX.txt') {
  tab <- fread(filename, sep = ';', dec  = ',', header = FALSE)
  names(tab) <- c("Ticker", "delme", "Date", "Time", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  tab[, Date := as.Date(Date, format = "%d.%m.%Y")]
  tab <- tab[, list(Date, OPEN, HIGH, LOW, CLOSE)]
  tab <- tab[, list("OPEN" = head(OPEN, 1), "HIGH" = max(HIGH), "LOW" = min(LOW), "CLOSE" = tail(CLOSE, 1)), by = Date]
tab
}

GetFinamRTSData <- function() {
  tab1 <- fread("D:\\RTSI\\RTSI_150101_161231.txt", sep = ';')
  tab2 <- fread("D:\\RTSI\\RTSI_170101_170715.txt", sep = ';')
  tab <- rbind(tab1, tab2)
  names(tab) <- c("Date", "Time", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  tab[, Date := as.Date(as.character(Date), format = "%Y%m%d")]
  tab <- tab[, list(Date, OPEN, HIGH, LOW, CLOSE)]
  tab <- tab[, list("OPEN" = head(OPEN, 1), "HIGH" = max(HIGH), "LOW" = min(LOW), "CLOSE" = tail(CLOSE, 1)), by = Date]
tab
}

GetFinamRTSDataFut <- function() {
  tab1 <- fread("D:\\RTSI\\SPFB.RTS_150101_161231.txt", sep = ';')
  tab2 <- fread("D:\\RTSI\\SPFB.RTS_170101_170715.txt", sep = ';')
  tab <- rbind(tab1, tab2)
  names(tab) <- c("Date", "Time", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  tab[, Date := as.Date(as.character(Date), format = "%Y%m%d")]
  tab <- tab[, list(Date, OPEN, HIGH, LOW, CLOSE)]
  tab <- tab[, list("OPEN" = head(OPEN, 1), "HIGH" = max(HIGH), "LOW" = min(LOW), "CLOSE" = tail(CLOSE, 1)), by = Date]
tab
}

GetFundDataInDay <- function(filename = 'D:\\RTSI\\RSX.txt') {
  tab <- fread(filename, sep = ';', dec  = ',', header = FALSE)
  names(tab) <- c("Ticker", "delme", "Date", "Time", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  tab[, Date := as.Date(Date, format = "%d.%m.%Y")]
  tab[, Time := as.POSIXct(paste(as.character(Date), Time))]
  tab <- tab[, list(Date, Time, OPEN, HIGH, LOW, CLOSE)]
  return(tab)
}

GetFinamRTSDataFut <- function() {
  tab1 <- fread("D:\\RTSI\\SPFB.RTS_150101_161231.txt", sep = ';')
  tab2 <- fread("D:\\RTSI\\SPFB.RTS_170101_170715.txt", sep = ';')
  tab <- rbind(tab1, tab2)
  names(tab) <- c("Date", "Time", "OPEN", "HIGH", "LOW", "CLOSE", "Volume")
  tab[, Date := as.Date(as.character(Date), format = "%Y%m%d")]
  tab[, Time := as.POSIXct(paste(as.character(Date), Time))]
  tab <- tab[, list(Date, Time, OPEN, HIGH, LOW, CLOSE)]
  return(tab)
}

StudyJoinedDatByDates <- function(largetabdet) {
  dates <- unique(largetabdet$Date.x)
  res <- data.frame(Date = as.Date(numeric()), kef = numeric(), r2 = numeric())
  for (adate in dates) {
    print(as.Date(adate))
    largetabred <- largetabdet[Date.x == adate, ]
	amod <- lm(CLOSE.x ~ CLOSE.y + 0, data = largetabred)
	kef <- coef(amod)
	r2 <- summary(amod)$r.squared
	res <- rbind(res, data.frame(Date = as.Date(adate), kef = kef, r2 = r2))
  }
  res
}