FFCompare <- function(num, instr, start, end) {
  tab1 <- LoadBp(instr, start.date = start, end.date = end,
                 candle.type = paste0("ComparisonFastPlaza", num),
                 storage.path = "D:")
  tab2 <- LoadBp(instr, start.date = start, end.date = end,
                 candle.type = paste0("ComparisonFTBot", num),
                 storage.path = "D:")
  tab3 <- InnerMergeDf(list(fp = tab1, fb = tab2), "Time")
  print(summary(tab3$fp.Ask - tab3$fb.Ask))
  print(summary(tab3$fp.Bid - tab3$fb.Bid))
  par(mfcol = c(2,1))
#  plot(tab3$fp.Ask, type = "l")
#  lines(tab3$fb.Ask, col = "red")
#  plot(tab3$fb.Bid, type = "l")
#  lines(tab3$fb.Bid, col = "red")
  plot(tab3$fp.Ask - tab3$fb.Ask, ylim = c(-100, 100))
  plot(tab3$fp.Bid - tab3$fb.Bid, ylim = c(-100, 100))
}
