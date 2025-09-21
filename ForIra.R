ModTab <- function(tab) {
  tab <- as.data.table(as.data.frame(tab))
  tab <- tab[, 1:5, with = FALSE]
  names(tab) <- c("Time", "Security", "Direction", "Price", "Volume")
print(head(tab$Time))
 # tab$Time <- as.character(paste(Sys.Date(), as.character(tab$Time)))
print(head(tab$Time))
  tab$Time <- as.POSIXct(tab$Time)
print(head(tab$Time))
  tab1 <- tab[grepl("USD", tab$Security), ]
  tab2 <- tab[grepl("Si", tab$Security), ]
  tab2$Price <- tab2$Price/1000
list(tab1 = tab1, tab2 = tab2)
}
PlCalc <- function(timeend, tlist) {
  tab1 <- as.data.table(tlist$tab1)
  tab2 <- as.data.table(tlist$tab2)
  if (nrow(tab1) > 0) {
    tab1 <- tab1[Time <= timeend, ]
  }
  if (nrow(tab2) > 0) {
    tab2 <- tab2[Time <= timeend, ]
  }
  #if (nrow(tab1) == 0) {return(NA)}
  if (nrow(tab1) > 0) {
    tab1[, val := ifelse(Direction == "Купля", Volume,-Volume)]
  }
  if (nrow(tab2) > 0) {
    tab2[, val := ifelse(Direction == "Купля", Volume, -Volume)]
  }
  finpos1 <- sum(tab1$val)
  if (nrow(tab2) > 0) {
    finpos2 <- sum(tab2$val)
  }
  prof1 <- -sum(tab1$Price*tab1$val)
  if (nrow(tab2) > 0) {
    prof2 <- -sum(tab2$Price*tab2$val)
  }
  price1 <- tab1$Price[nrow(tab1)]
  if (nrow(tab2) > 0) {
    price2 <- tab2$Price[nrow(tab2)]
  }
  if (nrow(tab2) > 0) {
    return(1000*(prof1 + prof2 + ifelse(finpos1>0, price1*abs(finpos1), -price1*abs(finpos1)) +
     ifelse(finpos2>0, price2*abs(finpos2), -price2*abs(finpos2))))
  } else {
    return(1000*(prof1 +ifelse(finpos1>0, price1*abs(finpos1), -price1*abs(finpos1))))
  }
}

#
layout(matrix(1:2))
plot(tab$Time, tab$Close)
for (j in 1:nrow(mtab)) {
  if (mtab$Direction[j] == "Купля") {
    points(mtab$Time[j], mtab$Price[j]*1000, col = "red", pch = 25)
  } else {
    points(mtab$Time[j], mtab$Price[j]*1000, col = "green", pch = 24)
  }
}
plot(tab$Time, rep(0, nrow(tab)), ylim = c(min(rres$res), max(rres$res)))
for (j in 1:nrow(rres)) {
  points(rres$Time[j], rres$res[j])
}
