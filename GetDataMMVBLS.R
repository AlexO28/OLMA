library(zoo)

tabMMVB <- LoadBp("MXH6@FORTS", start.date = as.Date("2015-12-13"), end.date = Sys.Date(), candle.type = "Vol5", storage.path = "D:\\ForSergey\\")
tabmmvb <- LoadBp("MMH6@FORTS", start.date = as.Date("2015-12-13"), end.date = Sys.Date(), candle.type = "Vol5", storage.path = "D:\\ForSergey\\")
tab <- InnerMergeDf(list(mix = as.data.frame(tabMMVB), mixsmall = as.data.frame(tabmmvb)), "Time")
tab <- na.omit(tab)
names(tab) <- tolower(names(tab))

tab$Time <- tab$mix.time
mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"))
tab <- tab[mask>0, ]

ind <- Purify2(tab, len = 900, strname = "mix.")
ind <- union(ind, Purify2(tab, len = 900, strname = "mixsmall."))
tab2 <- tab[-ind, ]
tab2$mixsmall.bid <- 100*tab2$mixsmall.bid
tab2$mixsmall.ask <- 100*tab2$mixsmall.ask
tab2$mix.close <- (tab2$mix.bid + tab2$mix.ask)/2
tab2$mixsmall.close <- (tab2$mixsmall.bid + tab2$mixsmall.ask)/2
tab2$diff <- tab2$mix.close - tab2$mixsmall.close
tab3 <- KillQuantiles(tab2, 0.05, "diff", 900)
tab3 <- tab3[tab3$diff >= -300, ]
tab3$date <- tab3$mix.date

#tab3$modelask <- tab3$mixsmall.ask + 100
#tab3$modelbid <- tab3$mixsmall.bid + 100

tab3$mixsmall.bid <- tab3$mixsmall.bid - 100
tab3$mixsmall.ask <- tab3$mixsmall.ask - 100
tab3$mixsmall.close <- tab3$mixsmall.close - 100

PlotGraphLS1 <- function(tab) {
  layout(matrix(1:2, nrow = 2))
  plot(tab$date, tab$mix.ask-tab$mixsmall.bid, type = "l", col = "red", ylab = "mmvb = small mmvb - 100", ylim = c(-200, 500))
  lines(tab$date, tab$mix.bid-tab$mixsmall.ask, col = "green")
#  legend("bottomright", legend = c("ask", "bid"), lty = 1, col = c("green", "red"))
  plot(1:nrow(tab), tab$mix.ask-tab$mixsmall.bid, type = "l", col = "red", ylab = "mmvb = small mmvb - 100", ylim = c(-200, 500))
  lines(1:nrow(tab), tab$mix.bid-tab$mixsmall.ask, col = "green")
 # legend("bottomright", legend = c("ask", "bid"), lty = 1, col = c("green", "red"))
}
PlotGraphLS2 <- function(tab) {
	layout(1)
  plot(1:nrow(tab), tab$mix.ask-tab$mixsmall.bid, type = "l", col = "red", ylab = "mmvb = small mmvb - 100", ylim = c(-600, 600))
  lines(1:nrow(tab), tab$mix.bid-tab$mixsmall.ask, col = "green")
  legend("bottomright", legend = c("ask", "bid"), lty = 1, col = c("green", "red"))
}

MainTestLS <- function(tab) {
  for (size in seq(25, 300, 25)) {
    for (shift in seq(0, size, 25)) {
      res <- Strategy_MMVBMMRegime(tab, data.frame(size = size, shift = shift, check = FALSE), delay = 0)
      vec <- c(size, shift, 0,
               mean(res$modelprofit), median(res$modelprofit), min(res$modelprofit), mean(res$numdeals), max(abs(res$curposes)))
      res2 <- Strategy_MMVBMMRegime(tab, data.frame(size = size, shift = shift, check = FALSE), delay = 1)
      vec2 <-  c(size, shift, 1,
               mean(res2$modelprofit), median(res2$modelprofit), min(res2$modelprofit), mean(res2$numdeals), max(abs(res2$curposes)))
      write.table(rbind(vec, vec2), "mmvbls.txt", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
    }
  }
}
