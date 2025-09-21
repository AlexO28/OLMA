options(stringsAsFactors = FALSE)
options(digits.secs = 3)
origin <- "1970-01-01"

Main <- function() {
  params <- read.table("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\inparams.txt", sep = "=", header = FALSE)
  filename1 <- params[params[, 1] == "filename1", 2]
  filename2 <- params[params[, 1] == "filename2", 2]
  secname1 <- params[params[, 1] == "secname1", 2]
  secname2 <- params[params[, 1] == "secname2", 2]
  src <- params[params[, 1] == "src", 2]
  comparewith <- params[params[, 1] == "comparewith", 2]
  mode <- params[params[, 1] == "mode", 2]
  writefile1 <- params[params[, 1] == "datfile1", 2]
  writefile2 <- params[params[, 1] == "datfile2", 2]

  if (mode == "studyone") {
    if (src == "Quick") {
      dat <- GetDataFromQuick(filename1, filename2, secname1, secname2)
    } else {
      dat <- GetData(filename1, filename2, secname1, secname2)
    }
    if (comparewith == "Deals") {
      deals <- HedgeComparison2(dat)
    } else {
      deals <- HedgeComparison4(dat)
    }
    mtab <- GetHedgeDiff(dat, deals)
    write.table(data.frame(diff = mtab$tab$diff), 
                paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile1), sep = ",",
                row.names = FALSE, col.names = FALSE)
    rrr <- Analyze(mtab)
  } else {
    rrr <- Compare2Traders(writefile1, writefile2)
  }
}
Compare2Traders <- function(writefile1, writefile2) {
  dat1 <- read.table(paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile1), header = FALSE, sep = ",")
  dat2 <- read.table(paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\", writefile2), header = FALSE, sep = ",")
  dat1 <- dat1[, 1]
  dat2 <- dat2[, 1]
  filename <- "\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\log.txt"
  write.table(paste("Comparing data from", writefile1, "and", writefile2), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  res1 <- wilcox.test(dat1, dat2, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean difference between 2 samples is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval of mean difference between 2 samples is", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
  dat1 <- dat1[dat1 < quantile(dat1, 0.95)]
  dat2 <- dat2[dat2 < quantile(dat2, 0.95)]
  res1 <- wilcox.test(dat1, dat2, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean difference between 2 samples without of top outliers is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval of mean difference between 2 samples without of top outliers is", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
}
GetDataFromQuick <- function(filename1, filename2, secname1, secname2) {
  deals <- read.table(filename1, header = TRUE, sep = ";")
  orders <- read.table(filename2, header = TRUE, sep = ";")
  deals <- deals[!is.na(deals$Time) & !is.na(deals$MS), ]
  orders <- orders[!is.na(orders$Time) & !is.na(orders$MS), ]

print(head(deals))
print(head(orders))

  deals$MS <- deals$MS %/% 1000
  deals$MS <- ifelse(deals$MS < 10, 10, deals$MS)
  deals$MS <- ifelse(deals$MS >= 100, as.character(deals$MS), paste0("0", as.character(deals$MS)))
  orders$MS <- orders$MS %/% 1000
  orders$MS <- ifelse(orders$MS < 10, 10, orders$MS)
  orders$MS <- ifelse(orders$MS >= 100, as.character(orders$MS), paste0("0", as.character(orders$MS)))
  deals$Date <- paste0(Sys.Date(), " ", deals$Time, ".", deals$MS)
  orders$Date <- paste0(Sys.Date(), " ", orders$Time, ".", orders$MS)

print(head(deals))
print(head(orders))


  deals$Volume <- ifelse(deals$Side == "Купля", 1, -1)*deals$Volume
  orders$Volume <- ifelse(orders$Side == "Купля", 1, -1)*orders$Volume

  deals$Date <- as.POSIXct(as.character(deals$Date), origin = origin)
  orders$Date <- as.POSIXct(orders$Date, origin = origin)

  deals <- deals[order(deals$Date), ]
  orders <- orders[order(orders$Date), ]

  deals <- deals[deals$Security == secname1, ]
  orders <- orders[orders$Security == secname2, ]

  orders$id <- 1:nrow(orders)

print(head(deals))
print(head(orders))

list(deals = deals, orders = orders)
}
GetDataFromQuickPart <- function(filename, secname, date = NA) {
  tab <- read.table(filename, header = TRUE, sep = ";")
  tab <- tab[!is.na(tab$Time) & !is.na(tab$MS), ]
  print(head(tab))
  tab$MS <- tab$MS %/% 1000
  tab$MS <- ifelse(tab$MS < 10, 10, tab$MS)
  tab$MS <- ifelse(tab$MS >= 100, as.character(tab$MS), paste0("0", as.character(tab$MS)))
  if (is.na(date)) {
    date <- Sys.Date()
  }
  print(date)
  tab$Date <- paste0(date, " ", tab$Time, ".", tab$MS)
  tab$Volume <- ifelse(tab$Side == "Купля", 1, -1)*tab$Volume
  tab$Date <- as.POSIXct(as.character(tab$Date), origin = origin)
  tab <- tab[order(tab$Date), ]
  tab <- tab[tab$Security == secname, ]
  tab$id <- 1:nrow(tab)  
tab
}
GetDataFromMetroplexPart <- function(filename, secname) {
  tab <- read.table(filename, header = TRUE, sep = ";")
  tab <- tab[order(tab$Date), ]
  tab <- tab[tab$Security == secname, ]
  tab$Volume <- tab$Volume*ifelse(tab$Side == "Buy", 1, -1)
  tab$Date <- as.POSIXct(tab$Date, origin = origin)
  tab$id <- 1:nrow(tab)
tab
}
GetData <- function(filename1, filename2, secname1, secname2) {
  deals <- read.table(filename1, header = TRUE, sep = ";")
  orders <- read.table(filename2, header = TRUE, sep = ";")
  deals <- deals[order(deals$Date), ]
  orders <- orders[order(orders$Date), ]
  deals <- deals[deals$Security == secname1, ]
  orders <- orders[orders$Security == secname2, ]
  deals$Volume <- deals$Volume*ifelse(deals$Side == "Buy", 1, -1)
  orders$Volume <- orders$Volume*ifelse(orders$Side == "Buy", 1, -1)
  deals$Date <- as.POSIXct(deals$Date, origin = origin)
  orders$Date <- as.POSIXct(orders$Date, origin = origin)
  orders$id <- 1:nrow(orders)
list(deals = deals, orders = orders)
}
HedgeComparison1 <- function(dat) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- FALSE
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(j)
    time0 <- as.POSIXct(deals$Date[j])
    ordersred <- orders[(!orders$matched) & (orders$Date >= time0) & (orders$Date <= time0 + 60), ]
    print(nrow(ordersred))    
    if (nrow(ordersred) > 0) {
      vol <- deals$Volume[j]
      ordersred2 <- ordersred[ordersred$Volume == -vol, ]
      if (nrow(ordersred2) > 0) {
        oid <- ordersred2$id[1]
        deals$hedgeid[j] <- oid
        orders$matched[orders$id == oid] <- TRUE
      } else {print("skipping")}
    }
  }
deals
}
HedgeComparison2 <- function(dat) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- FALSE
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(paste("starting", j))
    time0 <- as.POSIXct(deals$Date[j])
    ordersred <- orders[(!orders$matched) & (orders$Date >= time0) & (orders$Date <= time0 + 60), ]
 #   print(nrow(ordersred))    
 #   print(ordersred)
    if (nrow(ordersred) > 0) {
      vol <- deals$Volume[j]
      finpos <- which(ordersred$Volume*vol > 0)
      if (length(finpos) == 0) {
        finpos <- nrow(ordersred)
      } else {
        finpos <- finpos[1]
      }
  #    print(paste0("finpos is", finpos))
      ordersred <- ordersred[1:finpos, ]
      ordersred$cs <- cumsum(ordersred$Volume)
      chosenpos <- which(ordersred$cs == -vol)
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
HedgeComparison3 <- function(dat) {
  deals <- dat$deals
  orders <- dat$orders
  orders$matched <- 0
  deals$hedgeid <- NA 
  for (j in 1:nrow(deals)) {
    print(paste("starting", j))
    time0 <- as.POSIXct(deals$Date[j])
    vol <- deals$Volume[j]
    ordersred <- orders[(orders$Date >= time0) & (orders$Date <= time0 + 60) & 
                          (orders$matched + vol <= abs(orders$Volume) + 0.1) &
                          (orders$Volume*vol < 0), ]
    if (nrow(ordersred) > 0) {
      oid <- ordersred$id[1]
      deals$hedgeid[j] <- oid
      orders$matched[orders$id == oid] <- orders$matched[orders$id == oid] + abs(vol)
    } else {print("skipping")}
  }
deals
}
HedgeComparison4 <- function(dat) {
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
      if (ordersred$Volume[1]*vol < 0 & ordersred$matched + vol <= abs(ordersred$Volume) + 0.1) {
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
  
  filename <- "\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\log.txt"
  write.table("Started analysis", filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("Using", 100*val, "percent of available data"), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds is from", confint[1], "to", confint[2]), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  pict <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\pict_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  tab <- tab[tab$diff < as.numeric(quantile(tab$diff, 0.95)), ]
  res1 <- wilcox.test(tab$diff, conf.int = TRUE)
  confint <- as.numeric(res1$conf.int)
  val <- as.numeric(res1$estimate)
  write.table(paste("Mean estimate without of top outliers in seconds is", val), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
  write.table(paste("95%-percent confidence interval in seconds witout of top outliers is from", confint[1], "to", confint[2]), filename, row.names = FALSE, append = TRUE, col.names = FALSE)
  pict <- paste0("\\\\192.168.1.204\\share\\People\\Алексей\\CompaisonResults\\pictrobust_", atime, ".jpeg")
  jpeg(pict, width = 1900, height = 1200)
  plot(tab$Date, tab$diff, ylab = "Hedge time in seconds")
  dev.off()
  write.table(paste("Wrote data to", pict), filename, row.names = FALSE, col.names = FALSE, append = TRUE)
}	