# use entry signal as entry point (-1 for sell, 1 for buy)
# return list of two df (same length) wich contains trades-df for entry and
# trades.df for exit
# side.flag: -1 for SELL entry's only; 1 for BUY entry's only; 0 for both
# use.opposite.as.exit: should we use opposite entrysignal as signal for exit?
DataFrame_strategy <- function(df, mask, entry.signal, exit.signal, side.flag = 0, use.opposite.as.exit = FALSE) {
  n = nrow(df)
  side.mask <- ifelse(side.flag == 0, entry.signal != 0, entry.signal = side.flag)
  signals.points <- which(mask==1 & side.mask)
  entry.trades <- create.trades()
  exit.trades <- create.trades()
  for(i in signal.points) {
   # trade <-
  }
}
