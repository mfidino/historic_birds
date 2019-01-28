library(tidyverse)

dat <- read.csv("../data/arrival.csv", stringsAsFactors = FALSE)

# we need to drop the shore birds and whatnot.
sp_code <- read.csv("../data/species_codes.csv", stringsAsFactors = FALSE)
sp_hist <- read.csv("../data/species_life_history.csv", stringsAsFactors = FALSE)

to_go <- c("Anseriformes",
					 "Charadriiformes",
					 "Gaviiformes",
					 "Gruiformes",
					 "Pelecaniformes",
					 "Podicipediformes",
					 "Suliformes")

sp_drop <- sp_hist$species[which(sp_hist$order %in% to_go)]
dat <- dat[-which(dat$species %in% sp_drop),]

# we need to drop some of the less common species. Figure out which
#  of the observers saw a given species.
sp_count <- table(dat$species, dat$observer)

# Next step. Grab the species who were seen all three time periods.
all_three <- names(which(rowSums(sp_count>0) == 3))

dat <- dat[which(dat$species %in% all_three),]

# make year start at 0 for the minimum.
dat$year0 <- dat$year - min(dat$year)



dat %>% group_by(species) %>% 
	
	d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
									year=rep(1:10, 2),
									response=c(rnorm(10), rnorm(10)))

test <- dat %>% 
	group_by(species) %>% 
	nest() %>% 
	mutate(model = map(data, ~lm(julian ~ year0, data = .)))
	
map(test$model, summary)
