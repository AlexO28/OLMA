GetSdComparedInfo <- function(tab, Delta1, Delta2)
{
restab <- rbind(c(), c())
if (Delta1>=Delta2) {print("Wrong choice of Deltas!")}
else
{
	days <- unique(tab$day)
	for (day in days)
	{
		print(day)
		tabred <- tab[tab$day==day,]
		datestart <- tabred$date[1]
		datecut <- 900
		tabred <- tabred[tabred$date>=datestart+900,]
		intstart2 <- tabred$date[1]
		intstart1 <- tabred$date[1]
		endstart2 <- intstart2 + Delta2
		endstart1 <- intstart1 + Delta1
		while (endstart2<=tabred$date[dim(tabred)[1]])
		{
			tabslice <- tab[((tab$date>=intstart2) & (tab$date<=endstart2)),]
			vec <- (tabslice$bid + tabslice$ask)/2
			###val2 <- sd(vec)			
			val2 <- sum(diff(abs(vec)))
		
			while (endstart1 <= tabslice$date[dim(tabred)[1]])
			{
				tabpiece <- tabslice[((tabslice$date>=intstart1) & (tabslice$date<=endstart1)),]
				vec <- (tabpiece$bid + tabpiece$ask)/2
				###val1 <- sd(vec)
				val1 <- sum(diff(abs(vec)))

				restab <- rbind(restab, rbind(c(), c(day, intstart1, endstart1, intstart2, endstart2, val1, val2)))
				
				intstart1 <- endstart1
				endstart1 <- endstart1 + Delta1
			}

			intstart2 <- endstart2
			endstart2 <- endstart2 + Delta2
		}
	}
}
restab
}