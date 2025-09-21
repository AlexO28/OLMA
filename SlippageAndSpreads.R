GetSlippageAndSpreadsInfo <- function(tab) {
  print("spreads info")
  print(summary((tab$Ask - tab$Bid)/2))
  vecbid <- abs(diff(tab$Bid))
  vecask <- abs(diff(tab$Ask))
  vecbid <- vecbid[2:(length(vecbid)-1)]
  vecask <- vecask[2:(length(vecask)-1)]
  print("slippage info")
  print("for bids")
  print(summary(vecbid))
  print("for asks")
  print(summary(vecask))
}
