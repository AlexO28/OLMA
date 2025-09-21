LogProbFun <- function(params, x, y) {
  z <- numeric(length(x))
  z[y > 0] <- pnorm(x[y > 0] + y[y > 0], lower.tail = TRUE, log.p = TRUE, mean = params[1], sd = params[2])
  z[y <= 0] <- pnorm(x[y <= 0] + y[y <= 0], lower.tail = FALSE, log.p = TRUE, mean = params[1], sd = params[2])
-sum(z) 
}
Optimize <- function(x, y) {
#lower = c(-10, 0.1), upper = c(10, 10)
  res <- optim(c(0, 1), LogProbFun, x = x, y = y, lower = c(-10, 0.1), upper = c(10, 10))
#  res <- optim(c(0, 1), LogProbFun, x = x, y = y)
res
}
OptimCycle <- function(x, y) {
  res <- data.frame(id = numeric(), mu = numeric(), sd = numeric())
  for (j in 2:length(x)) {
    tres <- Optimize(x[1:j], y[1:j])
    res <- rbind(res, data.frame(id = j, mu = tres$par[1], sd = tres$par[2]))
  }
res
}