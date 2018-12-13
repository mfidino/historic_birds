# read in the height of migration data

height <- read.csv(".../data/species_seen_per_day.csv", 
									 stringsAsFactors = FALSE)

# excel screwed up the dates.
height$date <- c(ymd(height$date[1:28]), mdy(height$date[29:nrow(height)]))

height$year <- year(height$date)

# subset down to may
may <- height[month(height$date) == 5,]

# between 7 - 20
may <- may[between(day(may$date), 7, 20),]

may$observer <- NULL
may$observer[between(may$year, 1898, 1903)] <- "Walter"
may$observer[between(may$year, 1927, 1932)] <- "Dreuth"
may$observer[between(may$year, 2012, 2015)] <- "Fidino"
may$observer <- factor(may$observer, levels = c("Walter", "Dreuth", "Fidino"))

hieght_summary <- may %>% 
	group_by(observer, day(date)) %>% 
	summarise(mu = mean(speciesSeen),
						se = sqrt(var(speciesSeen)/length(speciesSeen)))
colnames(hieght_summary) <- c("observer", "day", "mu", "se")
hieght_summary$low <- hieght_summary$mu - hieght_summary$se
hieght_summary$high <- hieght_summary$mu +hieght_summary$se

tiff("./height_migration/height_migration_draft_plot.tiff",
		 height = 6, width = 6, units = "in", res = 600,
		 compression = "lzw")
par(c(2,2,0.5,0.5))

walter <- hieght_summary[hieght_summary$observer == "Walter",]
plot(walter$mu ~ c(walter$day - 0.1), ylim = c(0,70), bty = "l",
		 xlab = "Day in May", ylab = "Average species richness", pch = 16)
for(i in 1:nrow(walter)){
	lines(x = rep(walter$day[i]-0.1, 2), y = c(walter$low[i], walter$high[i]))
}
lines(walter$mu ~ c(walter$day-0.1), lty = 3)
dreuth <- hieght_summary[hieght_summary$observer == "Dreuth",]

lines(dreuth$mu ~ c(dreuth$day), lty = 6, col = "red")
points(dreuth$mu ~ c(dreuth$day), pch = 15, col = "red")
for(i in 1:nrow(dreuth)){
	lines(x = rep(dreuth$day[i], 2), y = c(dreuth$low[i], dreuth$high[i]), col = "red")
}

fidino <- hieght_summary[hieght_summary$observer == "Fidino",]

lines(fidino$mu ~ c(fidino$day+0.1), lty = 4, col = "blue")
points(fidino$mu ~ c(fidino$day+0.1), pch = 17, col = "blue")
for(i in 1:nrow(fidino)){
	lines(x = rep(fidino$day[i]+0.1, 2), y = c(fidino$low[i]+0.1, 
																						 fidino$high[i]+0.1), col = "blue")
}
legend("topleft",legend = c("1898 - 1903", 
															 "1927 - 1932",
															 "2012 - 2015"),
			 col = c("black", "red", "blue"),
			 pch = c(16, 15, 17), bty = "n")

dev.off()
