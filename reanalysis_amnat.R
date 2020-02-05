##################
#
# reanalysis for amnat
#
#
##################

library(lme4)
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

# Fit the model
m1 <- glmer(
	y ~ dreuth +  fidino * change + log_bm_scaled *fidino+
			diet_breadth *fidino + forstrat_breadth * fidino +
		(1+dreuth + fidino|species),
	family = binomial,
	data = ds_state,
	control=glmerControl(optimizer="bobyqa",
											 optCtrl=list(maxfun=2e5))
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
	test <- predict(mod, pframe)
	return(test)
	}

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



