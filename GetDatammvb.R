Sys.setenv(tz = "GMT")
tabMMVB <- LoadBp("MMH6@FORTS", start.date = as.Date("2015-12-16"), end.date = Sys.Date(), candle.type = "1s", storage.path = "D:")
tabRTS <- LoadBp("RIH6@FORTS", start.date = as.Date("2015-12-16"), end.date = Sys.Date(), candle.type = "1s", storage.path = "D:")
tabSI <- LoadBp("SIH6@FORTS", start.date = as.Date("2015-12-16"), end.date = Sys.Date(), candle.type = "1s", storage.path = "D:")

expinfo$fullname <- paste0("MX", expinfo$shortname, "@FORTS")
tabMMVB <- LoadBpVarInstrs(storage = "D:", candletype = "1m", expinfo = expinfo)
expinfo$fullname <- paste0("RI", expinfo$shortname, "@FORTS")
tabRTS <- LoadBpVarInstrs(storage = "D:", candletype = "1m", expinfo = expinfo)
expinfo$fullname <- paste0("SI", expinfo$shortname, "@FORTS")
tabSI<- LoadBpVarInstrs(storage = "D:", candletype = "1m", expinfo = expinfo)

tab <- InnerMergeDf(list(mix = as.data.frame(tabMMVB), rts = as.data.frame(tabRTS), si = as.data.frame(tabSI)), "Time")
tab <- na.omit(tab)
names(tab) <- tolower(names(tab))

tab$Time <- tab$mix.time
mask <- MaskByTime(tab, c("10:15:00", "14:15:00", "19:05:00"), c("13:45:00", "18:35:00", "22:45:00"), tz = "GMT")
#mask <- MaskByTime(tab, c("10:15:00", "14:15:00"), c("13:45:00", "18:35:00"), tz = "GMT")
#mask <- MaskByTime(tab, c("19:05:00"), c("23:40:00"), tz = "GMT")
tab <- tab[mask>0, ]

tab <- MainInitialization(tab, "Time")

ind1 <- Purify2(tab, len = 900, strname = "mix.")
ind2 <- Purify2(tab, len = 900, strname = "rts.")
ind3 <- Purify2(tab, len = 900, strname = "si.")

ind2 <- Purify2(tab, len = 15, strname = "rts.")
ind3 <- Purify2(tab, len = 15, strname = "si.")

tab2 <- tab[-union(ind1, union(ind2, ind3)), ]

hhh <- aggregate(tab2$mix.bid, by = list(tab2$mix.date), FUN = length)
tab3 <- tab2[!(tab2$mix.date %in% hhh[hhh[, 2] <= 20000, 1]), ]

tab3$mix.bid <- 100*tab3$mix.bid
tab3$mix.ask <- 100*tab3$mix.ask

tab3 <- FillModelInd(tab3, data.frame(alpha = 1.587214, beta = 0), control = 0)
tab3$diff <- tab3$mix.close - tab3$modelclose

#tab4 <- KillQuantiles(tab3, 0.05, "diff", 15)
tab4 <- KillQuantiles(tab3, 0.05, "diff", 15*60)

tab3 <- tab3[!(tab3$mix.date %in% c(as.Date("2015-02-14"),
                                    as.Date("2015-02-21"),
                                    as.Date("2015-03-07"),
                                    as.Date("2015-05-01"),
                                    as.Date("2015-09-26"),
                                    as.Date("2015-10-03"),
                                    as.Date("2015-10-10"))),]


beta1 <- GetRollingBetas(tab3, 30)
beta2 <- GetRollingBetas(tab3, 3330)
slippage <- 5
commis <- 3
tab3_30min <- AddAverageMovingBetaToModel(tab3, beta1, beta2, 0)
tab3_1week <- AddAverageMovingBetaToModel(tab3, beta1, beta2, 1)
tab3_02 <- AddAverageMovingBetaToModel(tab3, beta1, beta2, 0.2)
res1_30min <- Strategy_MMVBMMRegime(tab3_30min, data.frame(size = 125, shift = 100, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res1_1week <- Strategy_MMVBMMRegime(tab3_1week, data.frame(size = 125, shift = 100, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res1_02 <- Strategy_MMVBMMRegime(tab3_02, data.frame(size = 125, shift = 100, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res2_30min <- Strategy_MMVBMMRegime(tab3_30min, data.frame(size = 100, shift = 50, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res2_1week <- Strategy_MMVBMMRegime(tab3_1week, data.frame(size = 100, shift = 50, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res2_02 <- Strategy_MMVBMMRegime(tab3_02, data.frame(size = 100, shift = 50, delay = 0, check = TRUE), commis = commis, slippage = slippage)
res3_30min <- Strategy_MMVBMMRegime(tab3_30min, data.frame(size = 100, shift = 50, delay = 0, check = FALSE), commis = commis, slippage = slippage, regime = "arbitrage")
res3_1week <- Strategy_MMVBMMRegime(tab3_1week, data.frame(size = 100, shift = 50, delay = 0, check = FALSE), commis = commis, slippage = slippage, regime = "arbitrage")
res4_1week <- Strategy_MMVBMMRegime(tab3_1week, data.frame(size = 150, shift = 100, delay = 0, check = FALSE), commis = commis, slippage = slippage, regime = "arbitrage")
tab4_30min <- FillModelRTS(tab3_30min, data.frame(alpha = 1.587214, beta = 0))
tab4_1week <- FillModelRTS(tab3_1week, data.frame(alpha = 1.587214, beta = 0))
resrts <- Strategy_MMVBMMRegime(tab4_1week, data.frame(size = 50, shift = 25, check = FALSE), commis = commis, slippage = slippage, regime = "standard", assetname = "rts.")
resrts2 <- Strategy_MMVBMMRegime(tab4_1week, data.frame(size = 75, shift = 50, check = FALSE), commis = commis, slippage = slippage, regime = "standard", assetname = "rts.")



beta3 <- GetRollingBetas(tab3, 666)

plot(1:length(beta1), ylim = range(c(beta1, beta2))*1.2, type = "n")
lines(1:length(beta1), tab3$mix.close - tab3$modelclose)
lines(1:length(beta2), beta2, col = "blue")
lines(1:length(beta1), beta1, col = "red")
lines(1:length(beta2), beta2 + 108, col = "green")
lines(1:length(beta2), beta2 - 93, col = "green")
lines(1:length(beta2), beta2 + 266, col = "yellow")
lines(1:length(beta2), beta2 - 134, col = "yellow")
legend("topright", cex = 0.5, col = c("black", "blue", "red", "green", "yellow"), legend = c("market", "30 min", "5 days", "80%", "90%"), lty = 1)

legend("topright", cex = 0.5, col = c("black", "blue", "red"), legend = c("market", "30 min", "5 days"), lty = 1)

betam <- tab3$mix.close - tab3$modelclose - GetRollingBetas(tab3, 3330/2)
plot(1:length(beta1), ylim = range(betam)*1.2, type = "n")
lines(1:length(beta1), betam)
lines(1:length(beta2), rep(125, length(betam)), col = "green")
lines(1:length(beta2), rep(-125, length(betam)), col = "green")
lines(1:length(beta2), rep(250, length(betam)), col = "yellow")
lines(1:length(beta2), rep(-250, length(betam)), col = "yellow")

plot(1:nrow(tab4), type = "n", ylim = range(tab4$beta)*1.2)
lines(beta1, col = "red")
lines(beta2, col = "blue")
lines(tab4$beta)




