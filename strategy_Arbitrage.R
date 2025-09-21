ArbStrategy <- function(df, mask, center, max.posa, slippage, pos.function, ...) {
  options(stringsAsFactors=FALSE)
  trades <- create.trades()
  curpos <- 0
  for(r in 1:nrow(df)) {
    candle = df[r, ]
    if(mask[r] ==1) {
      diff.price <- center[r] -candle$Close
      dif.pos <- trunc(max.posa * do.call(pos.function, args=c(..., x=diff.price)) - curpos)
      if(dif.pos != 0)
      {
      	if(dif.pos > 0) {
      		trades <- rbind(trades, buy(candle, abs(dif.pos), slippage=slippage))
      		curpos <- curpos + abs(dif.pos)
      	} else {
      		trades <- rbind(trades, sell(candle, abs(dif.pos), slippage=slippage))
      		curpos <- curpos - abs(dif.pos)
      	}
      }
    }
    else if(mask[r] == 2) {
      trades <- rbind(trades, close.position(candle, curpos, slippage))
      curpos <- 0
    }

  }
  trades
}

#functions in R -> [-1,1] 1 means full position
#use Vectorize(func) if needed
step.function <- function(x, shift.size, shift.lot) {
	y = trunc(x / shift.size) * shift.lot
	y = max(-1,min(y,1))
	y
}





