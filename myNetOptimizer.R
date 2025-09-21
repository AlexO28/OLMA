instname0 <- "RIH6@FORTS"
startdate0 <- as.Date("2015-12-16")
enddate0 <- Sys.Date()

NetOptim <- function(tab, sizes, shiftmultips, NS, stepprice, stepval, commis, GO, filename = "optimresults.txt") {
  names(tab) <- tolower(names(tab))
  len <- length(unique(tab$date))
  write.table(rbind(c(), c("N", "size", "shift", "totalpl", "maxpos", "median", "numberofdeals", "annualizedtotalpl")),
              filename, col.names = FALSE, row.names = FALSE, append = FALSE, sep = ",")
  for (N in NS) {
    names(tab) <- tolower(names(tab))
    indik <- RollMeanIndik(tab, N)
    indik <- data.table(Date = tab$date, Val = indik)
    for (size in sizes) {
      for (shiftmultip in shiftmultips) {
        shift <- max(round(shiftmultip*size), 1)
        print(c(size, shift, N))
        res <- myNetStrategy(tab, indik, maxpos = 20,
                             params = list(size = size, shift = shift),
                             stepprice = stepprice, stepval = stepval,
                             commis = commis, GO = GO)
        maxpos <- res$maxpos
        totpl <- res$pl/maxpos
        midpl <- median(res$restab$pl)/maxpos
        numdeals <- sum(res$restab$numdeals)
        anntotpl <- 250*100*totpl/(len*maxpos)
        write.table(rbind(c(), c(N, size, shift, totpl, maxpos, midpl, numdeals, anntotpl)), filename,
                    col.names = FALSE, row.names = FALSE, append = TRUE, sep = ",")
      }
    }
  }
}

