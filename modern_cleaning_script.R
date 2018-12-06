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

# get minimum date per year for each species
md$Year <- year(md$Date)

arrival <- md %>% 
	group_by(ModernName, Year) %>% 
	summarise(arrival = min(Date))

write.csv(arrival, "modern_arrival.csv")

# get number of days each species was seen per year
unique_per_day <- md %>% 
	group_by(Date) %>% 
	do(data.frame(unique(.$ModernName), stringsAsFactors = FALSE))
