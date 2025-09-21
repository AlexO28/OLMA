GetData <- function() {
  ebsdat <- read.xlsx("C:\\OLMA\\Копия россия ебс бакс(1).xlsx", 3)

  ebsdat[1:1643, Time := paste("2016-10-10", Time)]
  ebsdat[1644:nrow(ebsdat), Time := (paste("2016-10-11", Time))]
  ebsdat[, Time := fastPOSIXct(Time)]
  print(head(ebsdat))
  ourdat <- LoadBp("USD000UTSTOM@CETS", start.date = as.Date("2016-10-10"),
  								 end.date = as.Date("2016-10-11"), candle.type = "1s",
  								 storage.path = "D:\\")
  ourdat <- ourdat[Time %in% ebsdat$Time, ]
#  comdat <- InnerMergeDf(list(ebs = as.data.frame(ebsdat), our = as.data.frame(ourdat)), "Time")
#list(comdat = comdat, ourdat = ourdat, ebsdat = ebsdat)
 cbind(ourdat, ebsdat)
}
QuatroPlot <- function(ebsdat) {
	ebsdat2 <- na.omit(data.frame(Time = ebsdat$Time,
												OurBid = ebsdat[,2],
												OurAsk = ebsdat[, 3],
												TheyBid = -(as.numeric(ebsdat[,7])-ebsdat[,2]),
												TheyAsk = -(as.numeric(ebsdat[,8]) - ebsdat[,3])))
	return(ebsdat2)
plot(1:nrow(ebsdat2), ebsdat2$OurAsk - ebsdat2$OurBid, col = "red", type = "l")
plot(1:nrow(ebsdat2), ebsdat2$TheyAsk - ebsdat2$OurBid, col = "blue")
lines(1:nrow(ebsdat2), ebsdat2$TheyBid - ebsdat2$OurBid, col = "green")
}