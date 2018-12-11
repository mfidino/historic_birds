library(lubridate)
library(dplyr)
library(reshape2)

dreuth_data <- read.csv("./data/DreuthData.csv",
												stringsAsFactors = FALSE)[,c("ID", "ModernName", "Date")]

# fix typos
dreuth_data[which(dreuth_data$ModernName == "american widgeon"),"ModernName"] <- "american wigeon"
dreuth_data[which(dreuth_data$ModernName == "blue-grey gnatcatcher"),"ModernName"] <- "blue-gray gnatcatcher"
dreuth_data[which(dreuth_data$ModernName == "bonaparte gull"),"ModernName"] <- "bonaparte's gull"
dreuth_data[which(dreuth_data$ModernName == "common golden-eye"),"ModernName"] <- "common goldeneye"
dreuth_data[which(dreuth_data$ModernName == "common loon \ncommon loon \t4/28/1927\ncommon loon \t4/29/1927\ncommon loon \t4/30/1927\ncommon loon"),"ModernName"] <- "common loon"
dreuth_data[which(dreuth_data$ModernName == "common night-hawk"),"ModernName"] <- "common nighthawk"
dreuth_data[which(dreuth_data$ModernName == "common yellow-throat"),"ModernName"] <- "common yellowthroat"
dreuth_data[which(dreuth_data$ModernName == "crow"),"ModernName"] <- "american crow"
dreuth_data[which(dreuth_data$ModernName == "eastern blue-bird"),"ModernName"] <- "eastern bluebird"
dreuth_data[which(dreuth_data$ModernName == "eastern wood peewee"),"ModernName"] <- "eastern wood-pewee"
dreuth_data[which(dreuth_data$ModernName == "eastern wood pewee"),"ModernName"] <- "eastern wood-pewee"
dreuth_data[which(dreuth_data$ModernName == "eastern-meadowlark"),"ModernName"] <- "eastern meadowlark"
dreuth_data[which(dreuth_data$ModernName == "grey-cheeked thrush"),"ModernName"] <- "gray-cheeked thrush"
dreuth_data[which(dreuth_data$ModernName == "henslow sparrow"),"ModernName"] <- "henslow's sparrow"
dreuth_data[which(dreuth_data$ModernName == "lincoln sparrow"),"ModernName"] <- "lincoln's sparrow"
dreuth_data[which(dreuth_data$ModernName == "red-headed woodpeckerf"),"ModernName"] <- "red-headed woodpecker"
dreuth_data[which(dreuth_data$ModernName == "northern shoveller"),"ModernName"] <- "northern shoveler"
dreuth_data[which(dreuth_data$ModernName == "northern parlua"),"ModernName"] <- "northern parula"
dreuth_data[which(dreuth_data$ModernName == "northern water thrush"),"ModernName"] <- "northern waterthrush"
dreuth_data[which(dreuth_data$ModernName == "northern water-thrush"),"ModernName"] <- "northern waterthrush"
dreuth_data[which(dreuth_data$ModernName == "oven bird"),"ModernName"] <- "ovenbird"
dreuth_data[which(dreuth_data$ModernName == "oven-bird"),"ModernName"] <- "ovenbird"
dreuth_data[which(dreuth_data$ModernName == "puruple finch"),"ModernName"] <- "purple finch"
dreuth_data[which(dreuth_data$ModernName == "red-headed woodpeckerf"),"ModernName"] <- "red-headed woodpecker"
dreuth_data[which(dreuth_data$ModernName == "sanderling \nsemi-palmated sandpiper\nsemi-palmated sandpiper"),"ModernName"] <- "sanderling"
dreuth_data[which(dreuth_data$ModernName == "vepser sparrow"),"ModernName"] <- "vesper sparrow"
dreuth_data[which(dreuth_data$ModernName == " northern flicker"),"ModernName"] <- "northern flicker"
dreuth_data[which(dreuth_data$ModernName == "northern flicker "),"ModernName"] <- "northern flicker"
dreuth_data[which(dreuth_data$ModernName == "rough-winged swallow"),"ModernName"] <- "northern rough-winged swallow"
dreuth_data[which(dreuth_data$ModernName == "wilson thrush"),"ModernName"] <- "northern rough-winged swallow"

# format date
dreuth_data$date_fix <- as.Date(dreuth_data$Date, "%m/%d/%Y")
# fix typos
year(dreuth_data[which(year(dreuth_data$date_fix) == "1027" |
                         year(dreuth_data$date_fix) == "0197" |
                   year(dreuth_data$date_fix) == "197"),"date_fix"]) <- 1927
year(dreuth_data[which(year(dreuth_data$date_fix) == "1920"),"date_fix"]) <- 1930
year(dreuth_data[which(year(dreuth_data$date_fix) == "1921"),"date_fix"]) <- 1931
# create year column
dreuth_data$year <- format(dreuth_data$date_fix, "%Y")
# fix a couple typos
year(dreuth_data[which(dreuth_data$year == "1027" | dreuth_data$year == "0197"),"date_fix"]) <- 1927
dreuth_data[which(dreuth_data$year == "1027" | dreuth_data$year == "0197"),"year"] <- format(as.Date("1927", "%Y"), "%Y")

dreuth_data$ModernName <- clean_names(dreuth_data$ModernName)

# summarize to get first arrival each year for each species
dreuth_arrival <- dreuth_data %>% 
  group_by(ModernName,year) %>% 
  summarize(first_arrive = min(date_fix)) %>%
  as.data.frame(.)
colnames(dreuth_arrival) <- c("CommonName", "Year", "ArrivalDate")

# summarize to get days detected for each year
dreuth_detections <- dreuth_data %>% 
  group_by(ModernName, year) %>% 
  summarize(days_detected = n_distinct(date_fix)) %>% 
  as.data.frame(., stringsAsFactors = FALSE)
colnames(dreuth_detections) <- c("CommonName", "Year", "DaysDetected")

dreuth_detections$ob <- "Dreuth"

# load data from Wild Birds in City Parks
# historical number of days seen
WBCP_detections <- read.csv("./data/HistoricNumberOfDaysSeen.csv", 
														stringsAsFactors = FALSE)

# function for correcting names in WBCP book
correctNames <- function(dat){
  # to lowercase
  dat$BooksCommonName <- tolower(dat$BooksCommonName)
  # match to Drueth dataset
  dat[which(dat$BooksCommonName == "robin"),"BooksCommonName"] <- "american robin"
  dat[which(dat$BooksCommonName == "cowbird"),"BooksCommonName"] <- "brown-headed cowbird"
  dat[which(dat$BooksCommonName == "flicker"),"BooksCommonName"] <- "northern flicker"
  dat[which(dat$BooksCommonName == "bluebird"),"BooksCommonName"] <- "eastern bluebird"
  dat[which(dat$BooksCommonName == "catbird"),"BooksCommonName"] <- "gray catbird"
  dat[which(dat$BooksCommonName == "phoebe"),"BooksCommonName"] <- "eastern phoebe"
  dat[which(dat$BooksCommonName == "crow"),"BooksCommonName"] <- "american crow"
  dat[which(dat$BooksCommonName == "meadowlark"),"BooksCommonName"] <- "eastern meadowlark"
  dat[which(dat$BooksCommonName == "chickadee"),"BooksCommonName"] <- "black-capped chickadee"
  dat[which(dat$BooksCommonName == "nighthawk"),"BooksCommonName"] <- "common nighthawk"
  dat[which(dat$BooksCommonName == "mockingbird"),"BooksCommonName"] <- "northern mockingbird"
  dat[which(dat$BooksCommonName == "cardinal"),"BooksCommonName"] <- "northern cardinal"
  dat[which(dat$BooksCommonName == "towhee"),"BooksCommonName"] <- "eastern towhee"
  dat[which(dat$BooksCommonName == "junco"),"BooksCommonName"] <- "dark-eyed junco"
  dat[which(dat$BooksCommonName == "wilson thrush"),"BooksCommonName"] <- "wilson's thrush"
  dat[which(dat$BooksCommonName == "tree sparrow"),"BooksCommonName"] <- "american tree sparrow"
  dat[which(dat$BooksCommonName == "blue golden-winged warblerblue golden-winged warbler"),"BooksCommonName"] <- "blue golden-winged warbler"
  return(dat)
}

WBCP_days <- correctNames(WBCP_detections)
WBCP_days$BooksCommonName <- clean_names(WBCP_days$BooksCommonName)


# convert to long form to match Dreuth data
book_long <- melt(WBCP_days[,2:8])
# match columnnames
colnames(book_long) <- c("CommonName", "Year", "DaysDetected")
book_long$ob <- "Walter"

# combine datasets
days_combo <- rbind(book_long, dreuth_detections)
# get rid of the X in the date variable for the book data
days_combo$Year <- gsub("X", "", days_combo$Year)

# back to wide form
days_wide <- dcast(days_combo, CommonName~Year)
# change NA's to 0
days_wide[is.na(days_wide)] <- 0

# load arrival dates
WBCP_arrival <- read.csv("./Data/HistoricTableofArrival2.csv", stringsAsFactors = FALSE)

# fix names
WBCP_arrival_fixed <- correctNames(WBCP_arrival)
WBCP_arrival_fixed$BooksCommonName <- clean_names(WBCP_arrival_fixed$BooksCommonName)
# format date
WBCP_arrival_fixed$DateFixed <- as.Date(WBCP_arrival_fixed$Date, "%m/%d/%Y")
WBCP_arrival_fixed$Year <- year(WBCP_arrival_fixed$DateFixed)

# combine datasets
# match columnnames
colnames(WBCP_arrival_fixed) <- c("ID", "CommonName", "Date", "ArrivalDate", "Year")
WBCP_arrival_fixed$ob <- "Walter"
dreuth_arrival$ob <- "Dreuth"
# combine in long form
arrival_combo <- rbind( WBCP_arrival_fixed[,c("CommonName", "Year", "ArrivalDate","ob")],
												dreuth_arrival)
arrival_combo$ArrivalDateJulian <- as.numeric(format(arrival_combo$ArrivalDate, format="%j"))
arrival_combo <- arrival_combo[complete.cases(arrival_combo),]
# to wide
arrival_wide <- dcast(arrival_combo[,c("CommonName","Year","ArrivalDateJulian")], 
                      CommonName~Year, value.var = "ArrivalDateJulian",
                      fun.aggregate = min, fill=0)
# change 0 to NA
arrival_wide[arrival_wide == 0] <- NA

# Note: the blue jay record creates an NA column, but can just be removed
# particularly if we are only using migrating species

# objects with data are 'days_combo', 'days_wide', 'arrival_combo', 'arrival_wide'

write.csv(arrival_combo, "./data/historic_arrival_to_join.csv")
write.csv(days_combo, "./data/historic_days_to_join.csv")
