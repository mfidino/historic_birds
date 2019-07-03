# read in the day seen data
ds <- read.csv("../data/days_seen_per_year.csv",
							 stringsAsFactors = FALSE)

# we need to drop the shore birds and whatnot.
sp_hist <- read.csv("../data/species_life_history.csv", stringsAsFactors = FALSE)

to_go <- c("Anseriformes",
					 "Charadriiformes",
					 "Gaviiformes",
					 "Gruiformes",
					 "Pelecaniformes",
					 "Podicipediformes",
					 "Suliformes")

sp_drop <- sp_hist$species[which(sp_hist$order %in% to_go)]
ds <- ds[-which(ds$species %in% sp_drop),]


sp_count <- table(ds$species, ds$observer)

# Next step. Grab the species who were seen in at least 2 periods
all_three <- names(which(rowSums(sp_count>0) >1 ))

ds <- ds[ds$species %in% all_three,]

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

sp_summary <- ds %>% group_by(species, observer, year) %>% 
	summarise(prop_seen = daySeen / countOfDays) %>% 
	ungroup() %>% group_by(species, observer) %>% 
	summarise(mu = mean(prop_seen), sd = sd(prop_seen))

names(coef_list) <- names(ds)
test <- data.frame(t(sapply(coef_list, '[[', 2)))
colnames(test) <- c('Walter', 'Dreuth', 'Fidino')
test$species <- names(ds)

coef_list <- data.frame(matrix(unlist(coef_list), ncol = 3, 
															 nrow = length(coef_list), byrow = TRUE))
coef_list$species <- names(ds)

my_probs <- coef_list
my_probs$X3 <- plogis(my_probs$X3 + my_probs$X1)
my_probs$X2 <- plogis(my_probs$X2 + my_probs$X1)
my_probs$X1 <- plogis(my_probs$X1)
increased <- which(apply(my_probs[,-4],1, function(x) x[1] < x[2] & x[2] & x[3]))

mod_summary[increased]

head(my_probs)

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
