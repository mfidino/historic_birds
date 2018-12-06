# cleaning the modern data

md <- read.csv("./data/Fidino_data.csv", header = TRUE,
							 stringsAsFactors = FALSE)

library(lubridate)
library(dplyr)

# get only transect 2 or NA
md <- md[md$Transect == 2 | is.na(md$Transect),]

# drop flyovers
md <- md[md$Flyover == 0,]

# drop some of the columns
md <- md[, colnames(md) %in% c("ModernName", "Date", "Time", "Location", "AmountSeen",
												 "Observer", "WeatherConditions")]

# modify date time

md$Date <- as.Date(mdy_hms(md$Date))

md$Time <- md$Time %>% 
	as.character(.) %>% # make character
	gsub("\\w\\w?/\\w\\w?/\\w{4} ","",.) %>%  #drop date stuff
	hms(.) # make hms class

# change colnames
colnames(md) <- c("ModernName", "Date", "Time", "Location", "n","Observer","Weather" )

# check the names
sort(unique(md$ModernName))

# make all lowercase
md$ModernName <- tolower(md$ModernName)

# drop modern with no species name
md <- md[-which(md$ModernName == ""),]

# drop species called delete
md <- md[-which(md$ModernName == "delete"),]

# drop dashes
md$ModernName <- gsub("-", " ", md$ModernName)

md$ModernName[md$ModernName == "domestic duck species"] <- "domestic duck"
md$ModernName[md$ModernName == "grackle"] <- "common grackle"
md$ModernName[md$ModernName == "domestic goose"] <- "toulouse goose (domestic goose breed)"
md$ModernName[md$ModernName == "pigeon"] <- "rock pigeon"
md$ModernName[md$ModernName == "toulouse goose (domestic goose breed)"] <- "domestic goose"
t(t(sort(unique(md$ModernName))))

which(year(md$Date) == 7314)
year(md$Date)[7314] <- 2014

md$Date[7314] <- mdy("3-1-2014")
md$Date[md$Date == "7314-03-01"] <- mdy("3-1-2014")

# find 2016 data

md[md$Year == 2016,]

# change 4/24/2018 to 4/24/2015
md$Date[md$Date == "2018-04-24"] <- mdy("4-24-2015")

# change 4/7/2018 to 4/7/2014
md$Date[md$Date == "2018-04-07"] <- mdy("4-7-2014")
md$Date[md$Date == "2018-04-17"] <- mdy("4-17-2015")
# change 3/11/2016 to 3/11/2014
md$Date[md$Date == "2016-03-11"] <- mdy("3-11-2014")
md$Date[md$Date == "2016-03-10"] <- mdy("3-10-2014")

md[which(year(md$Date) > 2015),]
# get minimum date per year for each species
md$Year <- year(md$Date)

arrival <- md %>% 
	group_by(ModernName, Year) %>% 
	summarise(arrival = min(Date))

write.csv(arrival, "./data/modern_arrival.csv")

# get number of days each species was seen per year
unique_per_day <- md %>% 
	group_by(Date) %>% 
	do(data.frame(unique(.$ModernName), stringsAsFactors = FALSE))

# add a year column again
unique_per_day$Year <- year(unique_per_day$Date)
colnames(unique_per_day) <- c("Date", "ModernName", "Year")

count_per_year <- unique_per_day %>% 
	group_by(ModernName, Year) %>% 
	summarise(DaysSeen = length(ModernName))

# make it a full table

unq_spe <- sort(unique(count_per_year$ModernName))
nyear <- 4
tmp <- data.frame(ModernName = rep(unq_spe, each = nyear),
									Year = rep(2012:2015, length(unq_spe)),
									stringsAsFactors = FALSE)

test <- left_join(tmp, count_per_year, by = c("ModernName" = "ModernName",
																							"Year" = "Year"))



range(count_per_year$days_seen)
