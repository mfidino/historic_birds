##################
#
# reanalysis for amnat
#
#
##################

library(lme4)
library(rstan)
library(rstanarm)
library(dplyr)

# read in the bird functional trait data
fun <- read.csv(
	"./data/BirdFuncDat.csv",
	stringsAsFactors = FALSE
)

# Fix up the names a bit to line up with our own data
fun$English <- gsub(
	"Grey",
	"Gray",
	fun$English
)
fun$English <- gsub(
	"grey",
	"gray",
	fun$English
)
fun$English <- gsub(
	"Wood-pewee",
	"Wood-Pewee",
	fun$English
)
fun$English <- gsub(
	"Treecreeper",
	"Creeper",
	fun$English
)
fun$English <- gsub(
	"coloured",
	"colored",
	fun$English
)
fun$English <- gsub(
	"Common Starling",
	"European Starling",
	fun$English
)
fun$English <- gsub(
	"Sand Martin",
	"Bank Swallow",
	fun$English
)
fun$English <- gsub(
	"Le Conte's Sparrow",
	"LeConte's Sparrow",
	fun$English
)
fun$English <- gsub(
	"Whip-poor-will",
	"Eastern Whip-poor-will",
	fun$English
)

ward <- read.csv(
	"./data/ward_changes.csv",
	stringsAsFactors = FALSE
)

# Do some slight changes to species names in the ward data
ward$species <- gsub(
	"N\\.",
	"Northern",
	ward$species
)

# combine to the lincoln park data
analysis <- "binomial"
source("prepare_data.R")

ds_state <- dplyr::left_join(
	ds,
	ward,
	by = "species"
)

# set NA values to 0. These are migrants.
ds_state$change[is.na(ds_state$change)] <- 0

# calculate the covariates we want for successful urban birds
# reduce down to the species in our pool

fun_trait_lp <- fun[
	which(
		fun$English %in% ds_state$species
	),
]

# remove some columns
fun_trait_lp <- fun_trait_lp %>% 
	group_by(English) %>% 
	select(English, Diet.Inv, Diet.Vend, Diet.Vect, Diet.Vfish,
				 Diet.Vunk, Diet.Scav, Diet.Fruit, Diet.Nect, Diet.Seed,
				 Diet.PlantO, ForStrat.watbelowsurf, ForStrat.wataroundsurf,
				 ForStrat.ground, ForStrat.understory, ForStrat.midhigh,
				 ForStrat.canopy, ForStrat.aerial, BodyMass.Value)

# calculate each species diet breadth
fun_trait_lp$diet_breadth <- 
	apply(
		fun_trait_lp[, grep("Diet", colnames(fun_trait_lp))],
		1,
		function(x) sum(x>0)
	)

# calculate each species breadth of foraging strategy
fun_trait_lp$forstrat_breadth <- 
	apply(fun_trait_lp[, grep("ForStrat", colnames(fun_trait_lp))],
				1,
				function(x) sum(x>0)
	)

fun_trait_lp <- fun_trait_lp %>% 
	select(English, BodyMass.Value, forstrat_breadth, diet_breadth)

# subtract one from forstart and diet breadth
fun_trait_lp$forstrat_breadth <- fun_trait_lp$forstrat_breadth -1
fun_trait_lp$diet_breadth <- fun_trait_lp$diet_breadth -1

ds_state <- dplyr::inner_join(
	ds_state, 
	fun_trait_lp,
	by = c("species" = "English")
)

# log body mass
ds_state$log_bm <- log(ds_state$BodyMass.Value)
# make species a factor
ds_state$species <- factor(ds_state$species)

# scale the covariates
ds_state$log_bm_scaled <- scale(ds_state$log_bm)
#ds_state$diet_scaled <- scale(ds_state$diet_breadth)
#ds_state$forstrat_scaled <- scale(ds_state$forstrat_breadth)

# increase the range of the change value so that the range
#  is more comparable to the scaled covariates.
ds_state$change <- ds_state$change * 10

# make the observer's into binary variables
#  doing so to allow for comparisons between
#  the most recent time period and the previous two
#  time periods.
ds_state$dreuth <- 0
ds_state$dreuth[ds_state$observer == "Dreuth"] <- 1
ds_state$fidino <- 0
ds_state$fidino[ds_state$observer == "Fidino"] <- 1

# create y matrix for the binomial response
y <- cbind(
	ds_state$daySeen,
	ds_state$countOfDays - ds_state$daySeen
)

test <- stan_glmer(
	y ~ dreuth +  fidino * change + log_bm_scaled *fidino+
			diet_breadth *fidino + forstrat_breadth * fidino +
			(1+dreuth + fidino|species),
	family = binomial(link = "logit"),
	prior = cauchy(0, 2.5),
	prior_intercept = cauchy(0, 5),
	QR = TRUE,
	chains = 6,
	cores = 6, 
	seed = 400,
	iter = 3000,
	data = ds_state
	)

saveRDS(test, "stan_output.RDS")

summary(test)

good_rhat <- refit(test, iter = 1000, chains = 6, seed = 12345)

posterior_interval(test, prob = 0.95)

launch_shinystan(test, ppd = FALSE)

# Fit the model
m1 <- glmer(
	y ~ dreuth +  fidino * change + log_bm_scaled *fidino+
			diet_breadth *fidino + forstrat_breadth * fidino +
		(1+dreuth + fidino|species),
	family = binomial,
	data = ds_state,
	control=glmerControl(optimizer="bobyqa")
)

m2 <- glmer(
	y ~ dreuth +  fidino * change + log_bm_scaled *fidino+
		diet_breadth *fidino + forstrat_breadth * fidino +
		(1+dreuth + fidino|species),
	family = binomial,
	data = ds_state,
	control=glmerControl(optimizer="bobyqa",
											 optCtrl=list(maxfun=2e4))
)

summary(m1)

# make predictions from model, we can use fun_trait_lp
#  to generate most of the prediction data.frame

for_preds <- left_join(fun_trait_lp,
											 ward,
											 by = c("English" = "species"))
for_preds$change[is.na(for_preds$change)] <- 0

make_preds <- function(obs, pframe = for_preds, mod){
	pframe$BodyMass.Value <- scale(log(pframe$BodyMass.Value)) 
	colnames(pframe)[1:2] <- c("species", "log_bm_scaled")
	pframe$change <- pframe$change * 10
	pframe$species <- factor(pframe$species)
	pframe$dreuth <- 0
	pframe$fidino <- 0
	if(obs == "Dreuth"){
		pframe$dreuth <- 1
	}
	if(obs == "Fidino"){
		pframe$fidino <- 1
	}
	#test <- predict(mod, pframe)
	#return(test)
	return(pframe)
}

longshot <- make_preds("Walter")


walter <- posterior_linpred(test, newdata = make_preds("Walter"))

fidino <- posterior_linpred(test, newdata = make_preds("Fidino"))

walter <- posterior_predict(test, newdata = make_preds("Fidino"))
walter <- apply(walter, 2, median)

wint <- predictive_interval(test, newdata = make_preds("Fidino"), prob = 0.95)


wdat <- make_preds("Fidino")
yo <- cbind(walter, wint)
yo <- yo[order(wdat$species),]
new_or <- order(yo[,1])

yo <- yo[new_or,]

wrdat <- ds_state[ds_state$observer == "Fidino",]  %>% group_by(species) %>% 
	summarise(prop = mean(daySeen ))


plot(yo[,1], ylim = c(0,70))
for( i in 1:121){
	lines(c(i,i),
				yo[i,-1])
}

points(wrdat$prop[new_or], pch = 16)


fidino <- posterior_predict(test, newdata = make_preds("Fidino"))
dreu <- posterior_predict(test, newdata = make_preds("Dreuth"))

walter <- plogis(walter)
fidino <- plogis(fidino)

ot_diff <- fidino-av




dreu <- plogis(dreu)


av <- (walter + dreu) / 2


ot_diff <- fidino - plogis(av)



ack <- apply(ot_diff, 2, median)


sp_ord <- order(ack)
my_species <- longshot$species[sp_ord]

plot(ack[order(ack)], ylim = c(-65,65),
		 ylab = "Difference in days observed: Fidino vs Mean(Walter & Dreuth)",
		 xlab = "Species")




lows <- apply(ot_diff, 2, quantile, probs = 0.025)
hihs <- apply(ot_diff, 2, quantile, probs = 0.975)
lows <- lows[order(ack)]
hihs <- hihs[order(ack)]
for(i in 1:121){
	lines(x = rep(i, 2), y = c(lows[i], hihs[i]))
}
abline(h = 0)

dat <- make_preds("Fidino")[order(ack),]

dat$species[which(lows>0)]

dat$species[which(hihs<0)]

differs <- c(which(hihs<0), which(lows>0))

plot(ack[order(ack)][differs], ylim = c(-40,60),
		 ylab = "Estimated difference in days observed: Fidino vs Mean(Walter & Dreuth)",
		 xlab = "Species",
		 bty = "l")

lows <- apply(ot_diff, 2, quantile, probs = 0.025)
hihs <- apply(ot_diff, 2, quantile, probs = 0.975)
lows <- lows[order(ack)][differs]
hihs <- hihs[order(ack)][differs]
for(i in 1:length(lows)){
	lines(x = rep(i, 2), y = c(lows[i], hihs[i]))
}
abline(h = 0)

hm <- make_preds("Fidino")

lil_move <- rnorm(121, 0, 0.1)

plot(ack ~ c(hm$diet_breadth+lil_move))
abline(h = 0)

for(i in 1:121){
	lines(x = rep(hm$diet_breadth[i] + lil_move[i], 2),
				y = c(lows[i], hihs[i]))
}


plot(ack ~ c(hm$forstrat_breadth+lil_move + 1))
abline(h = 0)

for(i in 1:121){
	lines(x = rep(hm$forstrat_breadth[i] + lil_move[i] + 1, 2),
				y = c(lows[i], hihs[i]))
}


#dropzeros
togo <- which(hm$change == 0)
min_df <- hm[-togo,]

plot(ack[-togo] ~ c(min_df$change / 10), ylim = c(-40,60))
cor(ack[-togo], min_df$change/10)
abline(h = 0)
for(i in 1:nrow(min_df)){
	lines(x = rep(min_df$change[i]/10, 2),
				y = c(lows[-togo][i], hihs[-togo][i]))
}



my_species[which(hihs < 0)]
my_species[which(lows > 0)]

ack <- posterior_predict(test, data.frame(longshot))

ack2 <- predictive_interval(test, prob = 0.95, data.frame(longshot))

apply(ack, 2, median)

walters <- bootMer(
	m1, 
  function(x) make_preds(obs = "Walter", mod = x),
	nsim = 500
)

saveRDS(walters, "walter_bootstrap.RDS")

ci_lower <- apply(
	walters$t,
	2,
	function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))
)

ci_upper <- apply(
	walters$t,
	2,
	function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE))
)

walter_pred <- data.frame(species = pframe$species,
								 est = walters$t0,
								 low = ci_lower,
								 upr = ci_upper)


dreuth <- bootMer(
	m1, 
	function(x) make_preds(obs = "Dreuth", mod = x),
	nsim = 500,
	parallel = "snow"
)

saveRDS(dreuth, "dreuth_bootstrap.RDS")

ci_lower <- apply(
	dreuth$t,
	2,
	function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))
)

ci_upper <- apply(
	dreuth$t,
	2,
	function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE))
)

plot(plogis(dreuth_pred$est) ~ plogis(walter_pred$est))

dreuth_pred <- data.frame(species = pframe$species,
													est = dreuth$t0,
													low = ci_lower,
													upr = ci_upper)

fidino <- bootMer(
	m2, 
	function(x) make_preds(obs = "Fidino", mod = x),
	nsim = 500,
	parallel = "snow"
)

saveRDS(fidino, "fidino_bootstrap.RDS")

ci_lower <- apply(
	fidino$t,
	2,
	function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))
)

ci_upper <- apply(
	fidino$t,
	2,
	function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE))
)

fidino_pred <- data.frame(species = pframe$species,
													est = fidino$t0,
													low = ci_lower,
													upr = ci_upper)

plot(plogis(fidino_pred$est) ~ plogis(walter_pred$est))

plot(plogis(fidino_pred$est[order(walter_pred$est, decreasing = TRUE)]))


hm <- fidino_pred
# get coefficients
mcof <- data.frame(coef(m1)$species)
x <- matrix(c(410938.07,4610657.19, 410964.72,4610152.84),
						ncol = 2, byrow = TRUE)


# get average change for each species
otime <- mcof[,c(1:3)] 
otime$dreuth <- otime$dreuth + otime$X.Intercept.
otime$fidino <- otime$fidino + otime$X.Intercept.

otime <- apply(otime, 2, plogis)

otime[big_change,]

# get comparisons across time




ss <- getME(m2,c("theta","fixef"))
m3 <- update(m2,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))

ss2 <- getME(m2, c("theta","fixef"))
m3 <- update(m2,start=ss2,control=glmerControl(optimizer="bobyqa",
																							 optCtrl=list(maxfun=2e6)))

co <- coef(yo)
bb <- glm(y ~ change + observer * species,
					data = test, family = binomial)



m3 <- update(yo,start=ss,control=glmerControl(optimizer="bobyqa",
																					 optCtrl=list(maxfun=2e5)))



