FillIdealOrders <- function(report2) {
  zero <- 100*(report2$modelAsk + report2$modelBid)/2
  report2$idealorderAsk <- zero - 100*report2$position/400 +100
  report2$idealorderBid <- zero - 100*report2$position/400 - 100
report2
}
GetPureProfit <- function(rltab, report2) {
  smallind <- which(rltab$security == "MMZ5@FORTS")
  res <- c()
  for (index in smallind) {
    if (rltab$direction[index] == "Sell") {
      res <- c(res, (rltab$price[index] - report2$modelAsk[index])*rltab$volume[index])
    } else {
      res <- c(res, (-rltab$price[index] + report2$modelBid[index])*rltab$volume[index])
    }
  }
list(index = smallind, res = res)
}
GetPureProfitRounded <- function(rltab, report2) {
  shifts <- - 100*report2$position/400
  smallind <- which(rltab$security == "MMZ5@FORTS")
  res <- c()
  dirs <- c()
  profs <- rep(NA, length(smallind))
  cc <- 1
  for (index in smallind) {
    if (rltab$direction[index] == "Sell") {
      #res <- c(res, (report2$orderAsk[index] - rltab$modelAsk[index] - shifts[index]))
      dirs <- c(dirs, -rltab$volume[index])
      #profs <- c(profs, (rltab$price[index] - rltab$modelAsk[index])*rltab$volume[index])
      #al <- (report2$orderAsk[index] - rltab$modelAsk[index])*rltab$volume[index]
      al <- (rltab$price[index] - rltab$modelAsk[index])*rltab$volume[index]
      profs[cc] <- al
    } else {
       #profs[cc] <- (-report2$orderBid[index] + rltab$modelBid[index])*rltab$volume[index]
      profs[cc] <- (-rltab$price[index] + rltab$modelBid[index])*rltab$volume[index]
       dirs <- c(dirs, rltab$volume[index])
       #res <- c(res, ( -rltab$orderBid[index] + rltab$modelBid[index] + shifts[index]))
    }
    cc <- cc + 1
  }
data.frame(dirs = dirs, profs = profs, ind = smallind, shift = shifts[smallind])
}
