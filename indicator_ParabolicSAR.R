#TA - parabolic
#https://en.wikipedia.org/wiki/Parabolic_SAR
Parabolic.SAR <- function(df, alpha, max.alpha = 0.2, step.alpha = 0.02) {
  l = nrow(df)
  is.long = TRUE
  change.point = FALSE
  #initialize
  cur.alpha = alpha
  SAR = numeric(l)
  SAR[1] = df$Low[1]
  high = df$High[1]
  low = df$Low[1]

  for(i in 2:l) {
    ep  <- ifelse(is.long,high,low)
    SAR[i] <- SAR[i-1] + cur.alpha * (ep - SAR[i-1])

    if(is.long & df$High[i] > high) {
      high <- df$High[i]
      cur.alpha <- min(cur.alpha + step.alpha, max.alpha)
    }

    if(!is.long & df$Low[i] < low) {
      low <- df$Low[i]
      cur.alpha <- min(cur.alpha + step.alpha, max.alpha)
    }

    #change trend direction?
    if((is.long && df$Low[i] < SAR[i])||(!is.long && df$High[i] > SAR[i])) {
      cur.alpha  <- alpha
      SAR[i] <- ep
      high <- df$High[i]
      low <- df$Low[i]
      is.long <- !is.long
    }
  }
SAR
}
