# if partial == FALSE, NA while i<n
RollMean <- function(x, n, partial = TRUE) {
  rollapply(x, width=n, FUN=mean, align="right",partial = partial, fill=NA)
}
RollMeanIndik <- function(tab, winlen) {
  vec0 <- (tab$bid + tab$ask)/2
  vec <-  rollmeanr(vec0, winlen)
  vec2 <- rollapplyr(vec0[1:winlen-1], winlen, mean, partial = TRUE)
  c(vec2, vec)
}

