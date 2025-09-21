# several common functions for work with data


# load default instrument parametrs
CreateInfo <- function(type="futures"){
  return(list(
    type=type,
    stepsize=1,
    stepprice=1,
    GO=1,
    commis=0,
    lotsize=1,
    point.price=1
  ))
}

# load some instrument parameters (GO, stepsize, etc)
LoadInfo <- function(instrument, file=file.path(getwd(),"..", "resources", "info.txt")) {
  data <- read.csv(file,sep="\t",skip=2,dec=",",stringsAsFactors = FALSE)
  plaza.match <- sapply(grepl, X = data$plaza, instrument)
  plaza.match[is.na(plaza.match)]<-F
  finam.match <- sapply(grepl, X = data$finam, instrument)
  finam.match[is.na(finam.match)]<-F
  index <- which(plaza.match | finam.match)
  if(length(index) == 1) {
    info <- data[index,]
    info$point.price <- info$stepprice/info$stepsize
    return(info)
  }
  return (NA)
}

# return expiration date by name
Expiration <- function(instrument, file=file.path(getwd(),"..", "resources", "expiration.txt")) {
	data = read.csv(file, header=FALSE, stringsAsFactors = FALSE)
	exp.date = data$V2[data$V1==instrument]
	if(length(exp.date)==1)
		return(dmy(exp.date))
	return(NULL)
}

# load your own data from RDS file from resources
LoadRds <- function(name) {
	file <- paste0(file.path(getwd(), "..", "resources", name), ".RDS")
	if(file.exists(file))
		return(readRDS(file))
	NA
}

# save some specific data to resources
SaveRds <- function(obj, file) {
	file <- file.path(getwd(), "..", "resources", paste0(file,".RDS"))
	saveRDS(obj,file, compress=TRUE)
}


#TO DO
# this function load result of metroplex own trades export and convert to ticks format
LoadOwnTrades <- function(file) {

}

#TO DO
# this function load result of metroplex own orders export and convert to ticks format
LoadOwnOrders <- function(file) {

}