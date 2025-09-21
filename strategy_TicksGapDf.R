# get sta from tick gaps
# ticks - df with modifyied trades
# mask - vector of T,F
# treshold
# side = "1 or -1 or 0 for both"
# forward.window = forward window in seconds
# sleep removes deals with time difference less than sleep (in seconds)

ticks.strategy <- function(ticks, mask, treshold, tp, side=0, forward.window, sleep=1) {
	signals <- as.logical(mask)
	if(side != 0)
		signals[ticks$Dir != side] <- F
	signals[abs(ticks$Worse - ticks$Best) < treshold] <- F

	result <- ticks[signals,]
	result$index <- which(signals)
	if(nrow(result)==0) return(result)
  	result <- result[c(Inf,diff(result$Time)) > sleep, ]
	  result$max <- -Inf
	  result$min <- Inf
	  result$last <- NA

	for(i in 1:nrow(result)) {
		index <- result$index[i] + 1
		while(index <= nrow(ticks) &&
					ticks$Date[index] == result$Date[i] &&
					result$Time[i] + forward.window > ticks$Time[index]) {
			if(ticks$Dir[index] ==  1) result$max[i] <- max(result$max[i], ticks$Worse[index])
			if(ticks$Dir[index] == -1) result$min[i] <- min(result$min[i], ticks$Worse[index])
			result$last[i] <- ticks$Price[index]
			index <- index + 1
		}
		#print(i)
	}

	result <- result[!(result$min == Inf | result$max == -Inf | is.na(result$last)),]

	if(nrow(result)==0) return(result)
	result$start <- result$Best + result$Dir * treshold
	result$profit <- result$Dir * (result$start - result$last)
	result$profit[result$Dir == 1 & result$min < (result$start - treshold * tp)] <- treshold * tp
	result$profit[result$Dir == -1 & result$max > (result$start + treshold * tp)] <- treshold * tp

	result
}
