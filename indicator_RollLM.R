RollLM <- function(x, n, partial = TRUE) {
  rollapply(x, width=n, FUN=mylm, align="right", partial = partial, fill=NA)
}
###ищем прямую наилучшего приближения
###выдаем значение этой прямой в конечный момента
mylm <- function(x) {
  v <- 1:length(x)
  l <- lm(x ~ v)
	predict(l, data.frame(v = length(x)+1))
}


RollLM.coeff <- function(x, n, partial = TRUE) {
	rollapply(x, width=n, FUN=coef.lm, align="right", partial = partial, fill=NA)

}
