#requires DollarStavkaGraphs.R, Research.R, DollarModelOnSwaps.R
GetHalfLifeTime <- function(datestart, dateend, curfutname, critdate, nextdate, returntab = FALSE) {
  tab <- FillStavkaTab(datestart, dateend, spotsec = "USD000UTSTOM@CETS", futsec = curfutname,
                       candletype = "Data1sec", storage.path = "\\\\192.168.1.204\\share\\People\\Алексей\\")
  tab <- PrepareModel(tab, critdate, nextdate)
  tab <- ModifyTheSwaps(tab)
  tab <- as.data.table(tab)
  if (returntab) {return(tab)}
  tab[, Val := stavka/newdiffs]
  res <- numeric(60)
  for (j in 0:59) {
    tabmin <- tab[seq(j+1, nrow(tab), 60), ]
    tabmin[, Valprev := c(0, tabmin$Val[1:(nrow(tabmin)-1)])]
    tabmin[, Valdiff := Val - Valprev]
    mymod <- lqs(tabmin$Valdiff ~ tabmin$Valprev)
    kefs <- coef(mymod)
    res[j+1] <- -log(2)/kefs[2]
    print(res)
  }
res
}
