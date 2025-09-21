GetValsForTask1 <- function(mu, theta1, theta2, T, N=239) {
  set.seed(N)
  errors = rnorm(T+2)
  vals = numeric(T)
  for (j in 1:T) {
    vals[j] = mu + theta1*errors[j+1] + theta2*errors[j] + errors[j+2] 
  }
return(vals)
}
Task2 <- function(mu, theta1, theta2, T) {
  vals <- GetValsForTask1(mu, theta1, theta2, T)
  plot(vals, xlab = "t", ylab = "y(t)", type = "l")
  abline(h = mu, col = "red")
  title("Values of MA(2)")
}
Task3 <- function(mu, theta1, theta2, T) {
  vals <- GetValsForTask1(mu, theta1, theta2, T)
  empir <- acf(vals)[1] 
  theoretical <- (theta1 + theta1*theta2)/(1+theta1^2+theta2^2)  
list(empir, theoretical)
}
#Task3 <- functiom(mu, theta1, theta2, T) {
#  vals <- GetValsForTask1(mu, theta1, theta2, T)
#  
#}
Task4 <- function(mu, theta1, theta2, T, N = 239) {
  resframe <- c()
  for (j in 1:30) {
    vals <- GetValsForTask1(mu, theta1, theta2, T, N = N - j + 1)
	resframe <- cbind(resframe, vals)
  }
  mus <- numeric(T)
  vars <- numeric(T)
  for (j in 1:T) {
    mus[j] <- mean(resframe[j, ])
	vars[j] <- sd(resframe[j, ])
  }
  plot(mus, ylim = c(min(c(mus, vars)), max(c(mus, vars))), type = 'l', col = 'red')
  lines(vars, col = 'blue')
  legend('topright', legend = c('means', 'variances'), fill = c('red', 'blue'))
  title('Plot of means and variances')
}
Task5 <- function(mu, theta1, theta2, T, N = 239) {
  resframe <- c()
  for (j in 1:30) {
    vals <- GetValsForTask1(mu, theta1, theta2, T, N = N - j + 1)
	resframe <- cbind(resframe, vals)
  }
  corrs <- numeric(T-1)
  for (j in 1:(T-1)) {
    corrs[j] <- cor(resframe[j, ], resframe[j+1, ])
  }
  plot(corrs, xlab = 't', type = 'l')
  title('Plot of autocorrelations')
}
