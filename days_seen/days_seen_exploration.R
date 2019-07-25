# read in the day seen data
ds <- read.csv("../data/days_seen_per_year.csv",
										stringsAsFactors = FALSE)

ds <- ds[-grep("House Sparrow", ds$species),]
# we need to drop the shore birds and whatnot.
sp_hist <- read.csv("../data/species_life_history.csv", stringsAsFactors = FALSE)[-194,]

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

ds <- split(ds, factor(ds$species))

mod_summary <-coef_list<- vector("list", length = length(ds))


library(car)
library(lsmeans)

has_difference <- rare <- relation <-  rep(NA, length(ds))



	

# see which species have gone up or down
for(i in 1:length(coef_list)){
	tmp <- ds[[i]]
	# drop if ALL zero
	my_sum <- tmp %>% group_by(observer) %>% 
		summarise(count = sum(daySeen))
	if(any(my_sum$count == 0)) {
		obs <- my_sum$observer[my_sum$count>0]
		tmp <- tmp[which(tmp$observer %in% obs),]
	}

	#tmp$yr0 <- tmp$year - min(tmp$year)
	tmp_y <- cbind(tmp$daySeen, tmp$countOfDays - tmp$daySeen)
	
	tmp_mod <- glm(tmp_y ~ observer, data = tmp,family = binomial)
	has_difference[i] <- Anova(tmp_mod)$`Pr(>Chisq)` < 0.05
	
	my_contrasts <- data.frame(lsmeans(tmp_mod, pairwise ~ observer)$contrasts)
	
	tmp_coefs <- coefficients(tmp_mod)

	# get standard errors
	to_pred <- data.frame(observer = factor(unique(tmp$observer),
																					levels = c("Walter","Dreuth", "Fidino")))

	
	#fits <- predict(tmp_mod, newdata = to_pred, type='link', se.fit = TRUE)
	fits <- predict(tmp_mod, newdata = to_pred, type='link', se.fit = TRUE)	
  
	lower <- plogis(qnorm(0.025, fits$fit, fits$se.fit))
	upper <- plogis(qnorm(0.975, fits$fit, fits$se.fit))
	
	to_return <- data.frame(species = names(ds)[i] ,est = plogis(fits$fit), lower, upper, 
													observer =to_pred$observer,
													rank = rank(plogis(fits$fit)))
	
	# do summary for when there is only 2 time observations
		if(!'Walter' %in% to_pred$observer){
			# walter stuff
			tmp_rank <- rank(c(0, to_return$est))
			if(my_contrasts$p.value < 0.05){
			relation[i] <- paste(tmp_rank, collapse = "-")
			} else {
				relation[i] <- "1-2-2"
			}
			next
		}
		if(!'Dreuth' %in% to_pred$observer){
			# Dreuth stuff
			tmp_rank <- rank(c(to_return$est[1], 0, to_return$est[2]))
			if(my_contrasts$p.value < 0.05){
				relation[i] <- paste(tmp_rank, collapse = "-")
			} else {
				relation[i] <- "2-1-2"
			}
			next	
		}
			if(!'Fidino' %in% to_pred$observer){
				# Dreuth stuff
				tmp_rank <- rank(c(to_return$est, 0))
				if(my_contrasts$p.value < 0.05){
					relation[i] <- paste(tmp_rank, collapse = "-")
				} else {
					relation[i] <- "2-2-1"
				}
				next
			}
	
	
	
	
	if(!has_difference[i]){
		relation[i] <- "1-1-1"
		rare[i] <- plogis(tmp_coefs[1]) < 0.05
		next
	}
	
	# check the order
	if(all(my_contrasts$p.value < 0.05)){
		relation[i] <- paste(to_return$rank, collapse = "-")
	}
	
	if(my_contrasts$p.value[1] > 0.05 & all(my_contrasts$p.value[-1] < 0.05)){
		to_return$est[1:2] <- to_return$est[1] 
		to_return$rank <- rank(to_return$est, ties.method = 'min')
	  relation[i] <- paste(to_return$rank, collapse = "-")
	}
	if(my_contrasts$p.value[1] < 0.05 & all(my_contrasts$p.value[-1] > 0.05)){
		if(my_contrasts$estimate[1] < 0){
			relation[i] <- '1-2-2'
		}
		if(my_contrasts$estimate[1] > 0){
			relation[i] <- '2-1-1'
		}
	}
	if( my_contrasts$p.value[2] < 0.05 & all(my_contrasts$p.value[-2] > 0.05) ){
		if(my_contrasts$estimate[2] < 0){
			relation[i] <- '1-1-2'
		}
		if(my_contrasts$estimate[2] > 0){
			relation[i] <- '2-2-1'
		}
	}
	if(my_contrasts$p.value[3] < 0.05 & all(my_contrasts$p.value[-3] > 0.05)){
		if(my_contrasts$estimate[3] < 0){
			relation[i] <- '1-1-2'
		}
		if(my_contrasts$estimate[3] > 0){
			relation[i] <- '2-2-1'
		}
	}
	
	
	
	if(my_contrasts$p.value[2] > 0.05 & all(my_contrasts$p.value[-2] < 0.05) ){
		to_return$est[c(1,3)] <- to_return$est[1] 
		to_return$rank <- rank(to_return$est, ties.method = 'min')
		relation[i] <- paste(to_return$rank, collapse = "-")
	}
	if(my_contrasts$p.value[3] > 0.05 & all(my_contrasts$p.value[-3] < 0.05) ){
		if(to_return$rank[1] == 1){
			to_return$rank <- c(1,2,2)
		}
		if(to_return$rank[1] == 3){
			to_return$rank <- c(2,1,1)
		}
		relation[i] <- paste(to_return$rank, collapse = "-")
	}
	if(all(my_contrasts$p.value > 0.05)){
		relation[i] <- '1-1-1'
		rare[i] <- plogis(tmp_coefs[1]) < 0.05
	}

	mod_summary[[i]] <- tmp_mod
	coef_list[[i]] <- to_return
}

x <- model.matrix(lg)

# Extract the coefficients from the model (a column vector with k entries)
beta <- matrix(lg$coefficients)

# CONTROL:

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


# do alpha diversity

# read in the day seen data
ds <- read.csv("../data/days_seen_per_year.csv",
							 stringsAsFactors = FALSE)

ds <- ds[-grep("House Sparrow", ds$species),]

# we need to drop the shore birds and whatnot.
sp_hist <- read.csv("../data/species_life_history.csv", stringsAsFactors = FALSE)[-194,]

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

hey <- ds %>% group_by(observer, species) %>%
	summarise(detected = sum(daySeen) / sum(countOfDays) )

alphas <- ds %>% group_by(observer, species) %>%
	summarise(detected = sum(daySeen) > 0 ) %>% 
	ungroup() %>% group_by(observer) %>% 
	summarise(a = sum(detected))

table(hey$observer)	

library(vegan)

sp_dat <- split(hey, hey$observer)




1 - vegdist(rbind(sp_dat$Dreuth$detected, sp_dat$Walter$detected), method = "jaccard")
1 - vegdist(rbind(sp_dat$Fidino$detected, sp_dat$Walter$detected), method = "jaccard")
1 - vegdist(rbind(sp_dat$Fidino$detected, sp_dat$Dreuth$detected), method = "jaccard")
walter_to_dreuth <- 1 - dist(rbind(sp_dat$Dreuth$detected, sp_dat$Walter$detected), "binary")
walter_to_fidino <- 1 - dist(rbind(sp_dat$Fidino$detected, sp_dat$Walter$detected), "binary")
0. <- 1 - dist(rbind(sp_dat$Fidino$detected, sp_dat$Dreuth$detected), "binary")

walter_alpha <- sum(sp_dat$Walter$detected)
dreuth_alpha <- sum(sp_dat$Walter$detected)

plot(sp_dat$Dreuth$detected ~ sp_dat$Walter$detected)




hm <- ds %>% group_by(observer, species) %>% 
	summarise(propSeen = sum(daySeen) / sum(countOfDays)) %>% 
	arrange(desc( propSeen)) %>% 
	arrange(observer) %>% 
	data.frame
hm$col <- "black"
hm$col[hm$observer == "Dreuth"] <- "red"	
hm$col[hm$observer == "Fidino"] <- "purple"	
hm$cou <- rep(1:146, 3)


ah <- sp_hist[which(sp_hist$species %in% ds$species),]

wal <- hm[which(hm$observer == "Walter"),]
wal <- wal[-which(wal$propSeen == 0),]

bt <- hm[which(hm$observer != "Walter"),]
bt <- bt[-which(bt$propSeen == 0),]

wal$species[which(!wal$species %in% bt$species)]

bt$species[which(!bt$species %in% wal$species)]

plot(hm$propSeen ~ hm$cou, col = hm$col)
