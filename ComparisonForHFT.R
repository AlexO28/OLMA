options(stringsAsFactors = FALSE)
options(digits.secs = 3)
origin <- "1970-01-01"

library(boot)

MAIN <- function(filename1, filename2, secname1, secname2, src, comparewith, mode, writefile1, writefile2, weights = NA, eps = 0.1) {
 if (mode == "studyone") {
    if (src == "Quick") {
      dat <- GetDataFromQuick(filename1, filename2, secname1, secname2, weights = weights)
    } else {
      dat <- GetData(filename1, filename2, secname1, secname2, weights = weights)
    }
    if (comparewith == "Deals") {
      deals <- HedgeComparison2(dat, eps = eps)
    } else {
      deals <- HedgeComparison4(dat, eps = eps)
    }
    mtab <- GetHedgeDiff(dat, deals)

    write.table(data.frame(diff = mtab$tab$diff), 
                writefile1, sep = ",",
                row.names = FALSE, col.names = FALSE)
    rrr <- Analyze(mtab)
  } else {
    rrr <- Compare2Traders(writefile1, writefile2)
  }
}

GetDataFromMetroplexPart <- function(filename, secname, weight = 1) {
  print(filename)
  print(secname)
  print(weight)
  tab <- read.table(filename, header = TRUE, sep = ";", comment.char = "")
  tab <- tab[order(tab$Date), ]
  tab <- tab[tab$Security == secname, ]
  tab$Volume <- weight*tab$Volume*ifelse(tab$Side == "Buy", 1, -1)
  tab$Date <- as.POSIXct(tab$Date, origin = origin)
  tab$id <- 1:nrow(tab)
tab
}
GetData <- function(filename1, filename2, secname1, secname2, weights = NA) {
  if (is.na(weights[1])) {weights <- c(1, 1)}
  deals <- GetDataFromMetroplexPart(filename1, secname1, weight = weights[1])
  orders <- GetDataFromMetroplexPart(filename2, secname2, weight = weights[2])
print(head(deals))
print(head(orders))
list(deals = deals, orders = orders)
}
HedgeComparison2 <- function(dat, eps = 0.1) {
print(eps)
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- FALSE
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(paste("starting 2", j))
    time0 <- as.POSIXct(deals$Date[j])
     ordersred <- orders[(!orders$matched) & (orders$Date >= time0) & (orders$Date <= time0 + 60), ]
    if (nrow(ordersred) > 0) {
      vol <- deals$Volume[j]
      finpos <- which(ordersred$Volume*vol > 0)
      if (length(finpos) == 0) {
        finpos <- nrow(ordersred)
      } else {
        finpos <- finpos[1]
      }
      ordersred <- ordersred[1:finpos, ]
      ordersred$cs <- cumsum(ordersred$Volume)
      chosenpos <- which(abs(ordersred$cs - (-vol))<=eps)
      if (length(chosenpos) > 0) {
        print(paste0("chosenpos is ", chosenpos))
        chosenpos <- chosenpos[1]
        oidmin <- ordersred$id[1]
        oid <- ordersred$id[chosenpos]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id >= oidmin & orders$id <= oid] <- TRUE
      } else {print("skipping")}
    }
  }
deals
}
HedgeComparison4 <- function(dat, eps = 0.1) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- 0
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(paste("starting", j))
    time0 <- as.POSIXct(deals$Date[j])
    vol <- deals$Volume[j]
    ordersred <- orders[(orders$Date >= time0) & (orders$Date <= time0 + 60), ]
    if (nrow(ordersred) > 0) {
      ordersred <- ordersred[1, ]
      if (ordersred$Volume[1]*vol < 0 & ordersred$matched + abs(vol) <= abs(ordersred$Volume) + 0.1 + eps) {
        oid <- ordersred$id[1]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id == oid] <- orders$matched[orders$id == oid] + abs(vol)
      } else {print("skipping")}
    }
  }
deals
}
GetHedgeDiff <- function(dat, deals) {
  orders <- dat$orders
  tot <- nrow(deals)
  deals <- deals[!is.na(deals$hedgeid), ]
  rel1 <- nrow(deals)/tot
  print(rel1)
  names(orders) <- tolower(names(orders))
  mtab <- cbind(deals, orders[deals$hedgeid, ]) 
  mtab$diff <- as.numeric(mtab$date - mtab$Date, units = "secs")
list(tab = mtab, val = rel1)
}
Analyze <- function(info) {
  tab <- info$tab
  val <- info$val
  atime <- Sys.time()
  atime <- gsub(" ", "_", as.character(atime))
  atime <- gsub(":", "-", atime)

  LOG <- paste0("log_", Sys.Date(), ".txt")
  filename <- paste0("C:\\CompaisonResults\\", LOG)  
  write.table("Started analysis", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Using", 100*val, "percent of available data"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds is from", confint[1], "to", confint[2]), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  pict <- paste0("C:\\CompaisonResults\\pict_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Summary is"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(rbind(as.numeric(summary(tab$diff)), c()), filename, row.names = FALSE, col.names = FALSE, append = TRUE)

  tab <- tab[tab$diff < as.numeric(quantile(tab$diff, 0.95)), ]
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate without of top outliers in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds witout of top outliers is from", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
  pict <- paste0("C:\\CompaisonResults\\pictrobust_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Summary without of top outliers is"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(rbind(as.numeric(summary(tab$diff)), c()), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  ###here we apply bootstrapping:
  bootstrapresult <- BootStrapMeanEstimate(tab$diff)
  write.table(paste("Bootstrap estimate", bootstrapresult$estimate), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Bootstrap confidence interval: from", bootstrapresult$conf.int[1], 
                    "to", bootstrapresult$conf.int[2]), 
                    filename, row.names = FALSE, col.names = FALSE, append = TRUE)   
}
BootStrapMeanEstimate <- function(vec) { 
  samplemean <- function(x, d) {
    mean(x[d])
  }

  bootdat <- boot(vec, samplemean, 10000)
  resvec <- bootdat$t
  alpha <- 0.025
  resvec <- resvec[resvec > as.numeric(quantile(resvec, alpha)) & resvec < as.numeric(quantile(resvec, 1-alpha))]
list(conf.int = c(min(resvec), max(resvec)), estimate = mean(resvec))
}

