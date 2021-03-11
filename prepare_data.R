########################################################
#
# This is a script to pull in the species data
#  and prepare it for both analyses.
#
# Written by M. Fidino
#
########################################################

##########################
# Subsetting the data
##########################

# read in the day seen data
ds <- read.csv(
	"./data/days_seen_per_year.csv",
	stringsAsFactors = FALSE
)

# remove the House Sparrow from this analysis
ds <- ds[-grep("House Sparrow", ds$species),]

# we need to drop the shore birds and whatnot.
#  Read in the species history data
sp_hist <- read.csv(
	"./data/species_life_history.csv",
	stringsAsFactors = FALSE
)

# These are the bird orders that must be removed
to_go <- c(
	"Anseriformes",
	"Charadriiformes",
	"Gaviiformes",
	"Gruiformes",
	"Pelecaniformes",
	"Podicipediformes",
	"Suliformes"
)

# These are the common names of the species that are to be removed
sp_drop <- sp_hist$species[which(sp_hist$order %in% to_go)]

# Remove them from this analysis
ds <- ds[-which(ds$species %in% sp_drop),]

# make a count of each species by survey period
sp_count <- table(
	ds$species,
	ds$observer
)

if(analysis == "binomial"){
# Next step. Grab the species who were seen in at least 2 periods
#  We can analyze species with birds in atleast 2 columns.
birds_to_analyze <- names(
	which(
		rowSums(sp_count>0) >1 
	)
)

# Keep only the species we can analyze
ds <- ds[ds$species %in% birds_to_analyze,]

}

###################################
# Prepare data for analysis
###################################

# Make complete data.frame (i.e, we need 0 values for years
#  a species was not seen, our current data does not have this.)

#  Get the unique year 
unq_year <- unique(
	ds$year
)
# and unique species
unq_species <- sort(
	unique(
		ds$species
	)
)

# a temporary data frame to fill in that is all unique combinations
#  of unq_year and unq_species
tmp <- expand.grid(
	species = unq_species,
	year = unq_year,
	stringsAsFactors = FALSE
)

# Left join into the tmp data.frame with data.frame ds
ds <- dplyr::left_join(
	tmp,
	ds,
	by = c(
		"species","year"
	)
)

# put in the observer in the correct year range
ds$observer[dplyr::between(ds$year, 1898, 1903)] <- "Walter"
ds$observer[dplyr::between(ds$year, 1927, 1932)] <- "Dreuth"
ds$observer[dplyr::between(ds$year, 2012, 2015)] <- "Fidino"

# convert to factor, ordered chronologically by observer
ds$observer <- factor(
	ds$observer,
	levels = c(
		"Walter", "Dreuth", "Fidino"
	)
)

# NA values in ds$daySeen are 0, as it indicates that species was
#  not observed on that year.
ds$daySeen[is.na(ds$daySeen)] <- 0

# read in the effort data. For analysis we need 
#  1) number of days a species was seen per year
#  2) the number of days an observer went out per year
effort <- read.csv(
	"./data/effort.csv",
	stringsAsFactors = FALSE
)

# join the two so that we can calculate proportion of days seen
ds <- dplyr::left_join(
	ds,
	effort,
	by = "year"
)

.to_post <- paste0(
	"The object you are now set to analyze is a data.frame named:\n\n",
	"\tds\n\n",
	"It stands for 'days seen'. This data.frame has 5 columns:\n",
	"\t1) species (character): Name of species\n",
	"\t2) year (integer): Year counts were conducted\n",
	"\t3) daySeen (numeric): Count of days a species was observed per year\n",
	"\t4) Observer (Factor): Represents survey period by last name\n",
	"\t5) countOfDays (integer): Count of days observer went out per year"
)
	
	cat(.to_post)
	rm(.to_post)

