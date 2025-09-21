candletype <- "1sVol1000"
datestart <- as.Date("2016-04-18")
dateend <- as.Date("2016-05-13")
instr <- "SIM6@FORTS"

tabSI <- LoadBp(instr, start.date = datestart, end.date = dateend, candle.type = candletype,
                storage.path = "D:\\")
tabSI <- as.data.frame(tabSI)
maskSI <- MaskByTime(tabSI, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"))
tabSI2 <- na.omit(tabSI[maskSI>0, ])
names(tabSI2) <- tolower(names(tabSI2))

DroninMegaScript <- function(datestart, dateend, candletype, instrname) {
  tab <- na.omit(LoadBp(instrname, datestart, dateend, candletype, storage.path = "D:\\"))
  print(c(quantile(tab$Ask - tab$Bid, 0.8),
        quantile(tab$Ask - tab$Bid, 0.9),
        quantile(tab$Ask - tab$Bid, 0.95)))
}
AshotMegaScript <- function(datestart, dateend, candletype, futsec, spotsec) {
  tab <- FillStavkaTab(datestart, dateend, futsec = futsec, spotsec = spotsec, candletype = candletype, type = "spot")
  filename <- paste0(candletype, id, "file.csv")
  write.table("dollar info", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("dollar spread", summary(1000*(tab$dol.ask - tab$dol.bid))), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("dollar quantiles", quantile(1000*(tab$dol.ask - tab$dol.bid), 0.8)), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("futures info", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("futures spread", summary(1000*(tab$si.ask - tab$si.bid))), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("futures quantiles", quantile(1000*(tab$si.ask - tab$si.bid), 0.8)), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table("cumulative info", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("futures spread", summary(tab$si.ask - tab$si.bid - 1000*tab$dol.ask + 1000*tab$dol.bid)), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste0("futures quantiles", quantile((tab$si.ask - tab$si.bid - 1000*tab$dol.ask + 1000*tab$dol.bid), 0.8)), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
}

tab <- FillStavkaTab(datestart, dateend, futsec = "SIM6@FORTS", candletype = candletype, type = "spot")

print("dollar")
print(summary(1000*(tab$dol.ask - tab$dol.bid)))
print(quantile(1000*(tab$dol.ask - tab$dol.bid), 0.8))
print("fut")
print(summary(tab$si.ask - tab$si.bid))
print(quantile(tab$si.ask - tab$si.bid, 0.8))
print("stavka")
print(summary(tab$si.ask - tab$si.bid - 1000*tab$dol.ask + 1000*tab$dol.bid))
print(quantile(tab$si.ask - tab$si.bid - 1000*tab$dol.ask + 1000*tab$dol.bid, 0.8))


tabres <- data.frame(Time = tabSI2$time, spread = tabSI2$ask - tabSI2$bid,
                     Ask = tabSI2$ask, Bid = tabSI2$bid)
