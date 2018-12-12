
# bring in the modern data
mod_day_seen <- read.csv("./data/species_number_days_seen_modern.csv", 
								stringsAsFactors = FALSE)
mod_day_seen$ModernName <- clean_names(mod_day_seen$ModernName)


mod_day_seen$ob <- "Fidino"
mod_arrival <- read.csv("./data/modern_arrival.csv",
												stringsAsFactors = FALSE)

mod_arrival$ob <- "Fidino"

# bring in the historic data

hist_day_seen <- read.csv("./data/historic_days_to_join.csv",
													stringsAsFactors = FALSE)

colnames(hist_day_seen) <- colnames(mod_day_seen)

# add zeros if needed

# change names to modern names

sp_table <- read.csv("./data/general_bird_info.csv",
										 stringsAsFactors = FALSE)
sp_table$ModernName[sp_table$ModernName == "lousiana water thrush"] <-
	"lousiana waterthrush"

sp_table$BooksCommonName <- clean_names(sp_table$BooksCommonName)
sp_table$ModernName <- clean_names(sp_table$ModernName)
unq_spe <- sort(unique(hist_day_seen$ModernName))

for(species in 1:length(unq_spe)){
	if(unq_spe[species] %in% sp_table$BooksCommonName){
		loc <- which(sp_table$BooksCommonName == unq_spe[species])
		hist_day_seen$ModernName[hist_day_seen$ModernName == unq_spe[species]] <-
			sp_table$ModernName[loc]
	} else {
		
		cat(paste(unq_spe[species], unq_spe[species] %in% sp_table$ModernName,  "\n"))
	}
}


unq_spe <- sort(unique(hist_day_seen$ModernName))
years <- sort(unique(hist_day_seen$Year))
	tmp <- data.frame(ModernName = rep(unq_spe, each = length(years)),
									Year = rep(years, length(unq_spe)),
									stringsAsFactors = FALSE)

day_seen <- left_join(tmp, hist_day_seen, by = c("ModernName" = "ModernName",
																									"Year" = "Year"))

day_seen$ob[between(day_seen$Year, 1898, 1903)] <- "Walter"
day_seen$ob[between(day_seen$Year, 1927, 1932)] <- "Dreuth"

dayseen <- rbind(hist_day_seen, mod_day_seen)
dayseen$DaysSeen[is.na(dayseen$DaysSeen)] <- 0

sort(unique(dayseen$ModernName))

test <- foreign::read.dbf("data/LIST18.DBF")

test$lowername <- clean_names(test$COMMONNAME)
test$lowername[test$lowername == "harriss sparrow"] <- "harris sparrow"

test <- data.frame(apply(test, 2, as.character), stringsAsFactors = FALSE)

test$COMMONNAME[2045] <- "Golden-winged Warbler"

write.csv(test, "./data/species_codes.csv", row.names = FALSE)

hm <- left_join(dayseen, test[,c("COMMONNAME", "lowername")],
								by = c("ModernName" = "lowername"))

unq_spe <- sort(unique(hm$COMMONNAME))

write.csv(dayseen, "./data/clean_days_seen_per_year.csv", row.names = FALSE)
