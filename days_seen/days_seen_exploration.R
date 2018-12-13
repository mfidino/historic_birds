# read in the day seen data
ds <- read.csv("../data/days_seen_per_year.csv",
										stringsAsFactors = FALSE)


# fill in zeroes for days not seen
unq_year <- unique(ds$year)
unq_species <- sort(unique(ds$species))
tmp <- data.frame(species = rep(sort(unq_species), each =length(unq_year)),
									year = rep(unq_year, length(unq_species)))

ds <- left_join(tmp, ds, by = c("species" = "species",
																"year" = "year"))
ds$observer[between(ds$year, 1898, 1903)] <- "Walter"
ds$observer[between(ds$year, 1927, 1932)] <- "Dreuth"
ds$observer[between(ds$year, 2012, 2015)] <- "Fidino"
ds$observer <- factor(ds$observer, levels = c("Walter", "Dreuth", "Fidino"))
ds$daySeen[is.na(ds$daySeen)] <- 0




# read in the effort data
effort <- read.csv("../data/effort.csv",
									 stringsAsFactors = FALSE)

# join the two so that we can calculate proportion of days seen

ds <- left_join(ds, effort, by = "year")

# calculate the proportion
ds$prop <- ds$daySeen / ds$countOfDays

# order better
ds <- ds[order(ds$species, ds$year),]

ds <- ds[,c("species", "year", "observer", "prop")]

spe_cor <- rep(NA, length(unq_species))

# see which species have gone up or down
for(i in 1:length(spe_cor)){
	tmp <- ds[ds$species == unq_species[i],]
	
	tmp_mod <- lm(prop ~ year, data = tmp)
	
	spe_cor[i] <- coef(tmp_mod)[2]
	
}


winners <- which(spe_cor > 0.0025)
unq_species[winners]

hm <- ds[ds$species == "Northern Cardinal",]
plot(hm$prop ~ hm$year, type = 'p')

hm <- ds[ds$species == "Downy Woodpecker",]
plot(hm$prop ~ hm$year, type = 'p')

for(i in 1:length(winners)){
	hm <- ds[ds$species == unq_species[winners[i]],]
	plot(hm$prop ~ hm$year, type = 'p', main = unq_species[winners[i]], ylim = c(0,1),
			 bty = "l", xlab = "year", ylab = "Proprtion of days seen March - May")
	
}


losers <- which(spe_cor < -0.0025)
unq_species[losers]

hm <- ds[ds$species == "Great Blue Heron",]
plot(hm$prop ~ hm$year, type = 'p')

hm <- ds[ds$species == "Cooper's Hawk",]
plot(hm$prop ~ hm$year, type = 'p')

for(i in 1:length(losers)){
	hm <- ds[ds$species == unq_species[losers[i]],]
	plot(hm$prop ~ hm$year, type = 'p', main = unq_species[losers[i]], ylim = c(0,1),
			 bty = "l", xlab = "year", ylab = "Proprtion of days seen March - May")
	
}


# figure out which species dropped 
test <- ds %>% group_by(species, observer) %>% 
	summarise(prop = sum(prop))


no_mason <- test$species[test$prop == 0 & test$observer == "Fidino"]

data.frame(test[test$species %in% no_mason,])

# figure out new species

test <- ds %>% group_by(species, year > 2000) %>% 
	summarise(prop = sum(prop))
colnames(test)[2] <- "mason"
yes_mason <- test$species[test$prop == 0 & test$mason != TRUE]
data.frame(test[test$species %in% yes_mason,])
