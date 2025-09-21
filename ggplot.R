#TO DO implement interface functions
PlotCandles <- function(df, col) {
	df$id <- 1:length(df$Time)
	ggplot(data = df, aes(x=id, y=Close)) + geom_line(colour = col) + xlab("id") + ylab("Price") + ylim(min(df$Close), max(df$Close))
}
PlotBidAsks <- function(df, col1, col2) {
	minval <- min(df$Bid)
	maxval <- max(df$Ask)
	df$id <- 1:length(df$Time)
	ggplot(data=df, aes(x=id, y=Bid)) + xlab("id") + ylab("Price") + ylim(minval, maxval) + geom_line(colour=col1) +
		geom_line(colour=col2, aes(x=id, y=Ask))
}
PlotTrades <- function(trades) {
	####trades$symb <- ifelse(trades$Dir>0, 24, 25)
  minval <- min(trades$Price) - 1
	maxval <- max(trades$Price) + 1
	tradesred <- trades[trades$Dir>0, ]
	flag <- FALSE
  p <- NULL
	if (nrow(tradesred) > 0) {
		flag <- TRUE
		p <- ggplot(data=tradesred, aes(x=Time, y=Price)) + xlab("Time") + ylab("Price") + ylim(minval, maxval) +
			geom_point(colour = "green", shape = 24, size = 4, fill = "green")
	}
  tradesred <- trades[trades$Dir<0, ]
  if (nrow(tradesred) > 0) {
  	if (flag) {
  	  p <- p + geom_point(data = tradesred, colour = "red", fill = "red", shape = 25, size = 4, aes(x = Time, y = Price))
  	} else {
    	p <- ggplot(data=tradesred, aes(x=Time, y=Price)) + xlab("Time") + ylab("Price") + ylim(minval, maxval) +
    		geom_point(colour = "red", shape = 25, size = 4, fill = "red")
  	}
  }
p
}
###indicator is a data-frame with columns Time and Val
PlotIndicator <- function(ind, col, control = "main", ylimits = NA) {
	ind$id <- 1:length(ind$Time)
  if (control == "main") {
    if (is.na(ylimits)) {
    	res <- ggplot(data = ind, aes(x=id, y=Val)) + geom_line(colour = col) + xlab("id") + ylab("Indicator price") +
	  	    ylim(min(ind$Val), max(ind$Val))
    } else {
      res <- ggplot(data = ind, aes(x=id, y=Val)) + geom_line(colour = col) + xlab("id") + ylab("Indicator price") +
        ylim(ylimits[1], ylimits[2])
    }
  } else {
    res <-  geom_line(colour = col, data = ind, aes(x=id, y=Val))
  }
res
}
###code is taken from here
###http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)

	numPlots = length(plots)

	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
										 ncol = cols, nrow = ceiling(numPlots/cols))
	}

	if (numPlots==1) {
		print(plots[[1]])

	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
																			layout.pos.col = matchidx$col))
		}
	}
}
