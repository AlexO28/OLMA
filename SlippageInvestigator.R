#requires Comparison.R for bootstrapping
BenderSlipInvestigator <- function(benderdeals) {
  slipdol <- benderdeals[Security == "USD000UTSTOM@CETS", slippage]
  slipfut <- benderdeals[Security == "SIU6@FORTS", slippage]
  print(summary(slipdol))
  print(summary(slipfut))
  bootstrapdol <- BootStrapMeanEstimate(slipdol)
  bootstrapfut <- BootStrapMeanEstimate(slipfut)
  print(c(bootstrapdol$conf.int[1], bootstrapdol$conf.int[2]))
  print(c(bootstrapfut$conf.int[1], bootstrapfut$conf.int[2]))
}
ArbitrazhyorSlipInvestigator <- function(arbslipinfo) {
  bootstrapfut <- BootStrapMeanEstimate(arbslipinfo$slippage)
  print(c(bootstrapfut$conf.int[1], bootstrapfut$conf.int[2]))
}
MedTimeInvestigator <- function(deals, sec) {
  deals <- as.data.table(as.data.frame(deals))
  deals <- deals[Security == sec, ]
  mytimes <- diff(as.numeric(deals$Time))
  print(summary(mytimes))
  bootstraptime <- BootStrapMeanEstimate(mytimes)
  print(c(bootstraptime$conf.int[1], bootstraptime$conf.int[2]))
}
BenderSlipInvestigatorHit <- function(deals, sec, side) {
  deals <- as.data.table(as.data.frame(deals))
  deals <- deals[Security == sec & Direction == side, ]
#  print(summary(deals))
  if (side == "Buy") {
    deals[, Slippage := (Price - deals$Price[1])/abs(Volume)]
  } else {
    deals[, Slippage := (deals$Price[1] - Price)/abs(Volume)]
  }
  print(summary(deals$Slippage))
  bootstrapslip <- BootStrapMeanEstimate(deals$Slippage)
  print(c(bootstrapslip$conf.int[1], bootstrapslip$conf.int[2]))
}
