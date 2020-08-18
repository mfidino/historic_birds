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

m1 <- stan_glmer(
	y ~ dreuth +  fidino * change + log_bm_scaled * fidino +
			diet_breadth * fidino + forstrat_breadth * fidino +
			(1 + dreuth + fidino|species),
	family = binomial(link = "logit"),
	prior = cauchy(0, 2.5),
	prior_intercept = cauchy(0, 5),
	QR = TRUE,
	chains = 6,
	cores = 6, 
	seed = 300,
	iter = 5000,
	data = ds_state,
	adapt_delta = 0.995
	)


msum <- summary(m1)

# get mean and 95% CI
mci <- cbind(
	m1$coefficients,
	posterior_interval(m1)[1:length(m1$coefficients),])

write.csv(
	mci,
	"posterior_intervals.csv"
)

hoo <- as.matrix(m1)

launch_shinystan(m1, ppd = FALSE)

# Fit the model

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

# generate predictions from linear predictor
walter <- posterior_linpred(
	m1,
	newdata = make_preds("Walter")
)
dreuth <- posterior_linpred(
	m1,
	newdata = make_preds("Dreuth")
)

# take average between the two
historical <- (walter + dreuth) / 2

fidino <- posterior_linpred(
	m1,
	newdata = make_preds("Fidino")
)

my_diff <- plogis(fidino) - plogis(historical)

my_diff <- apply(
	my_diff,
	2,
	quantile, probs = c(0.025, 0.5, 0.975)
) %>% t

row.names(my_diff) <- make_preds(obs = "Fidino")$species

my_diff <- my_diff[order(my_diff[,2]),]

my_diff <- my_diff * 100

# reduce down to significant effects
results <- my_diff[my_diff[,3] < 0 | my_diff[,1] > 0, ]

# set it up to plot
to_plot <- left_join(
	data.frame(species = row.names(results),
						 med = results[,2],
						 low = results[,1],
						 hi = results[,3]),
	ward,
	by = "species"
)

to_plot$change[is.na(to_plot$change)] <- 0
to_plot$col <- "gray70"
to_plot$col[which(to_plot$change > 0)] <- "black"
to_plot$col[which(to_plot$change < 0)] <- "white"
to_plot$pch <- 21
to_plot$pch[which(to_plot$change > 0)]  <- 24
to_plot$pch[which(to_plot$change < 0)]  <- 25

quartz(height = 6.5, width = 6.5)

pdf(
	"species_relative_change.pdf",
	height = 6.5,
	width = 6.5,
)
par(mar = c(1,5,1,0.25))
{plot(
	1~1,
	type = "n",
	bty = "l",
	axes = FALSE,
	xlab = "",
	ylab = "",
	xlim = c(1,57),
	ylim = c(-50,100)
)
	
	# put in names for biggest winners and losers
	
	losers <- head(results)[1:5,2]
	# Painstakingly putting the names in the figure
	
	loser_names <- names(losers)
	
	
	temp <- legend(
		5,
		-25 ,
		legend = rev(loser_names), 
		bty = "n",
		cex = 0.75,
		y.intersp = 1.3
	)
	
	for(i in 1:5){
		lines(
			x = c(i, rev(temp$text$x)[i] - 0.25),
			y = c(losers[i], rev(temp$text$y)[i]),
			col = "gray50"
		)
	}
	
	winners <- tail(results,5)[,2]
	# Painstakingly putting the names in the figure
	
	winner_names <- names(winners)
	
	temp <-legend(
		51,
		93 ,
		legend = rep(" ", 5), 
		text.width = strwidth("Black-capped Chickadee"),
		bty = "n",
		xjust = 1,
		yjust = 1,
		cex = 0.75,
		y.intersp = 1.3
	)
	
	text(temp$rect$left + temp$rect$w, temp$text$y,
			 rev(winner_names), pos = 2, cex = 0.75)
	
	
	for(i in 1:5){
		lines(
			x = c(50.2, i + 52),
			y = c(rev(temp$text$y)[i], winners[i]),
			col = "gray50"
		)
	}
	


# add confidence intervals
for(i in 1:nrow(results)){
	lines(
		x = c(i,i),
		y = results[i,-2],
		lwd = 2
	)
}
	


points(
	to_plot$med,
	pch = to_plot$pch,
	bg = to_plot$col
)

lines(
	x = c(-1,57),
	y = c(0,0),
	lty = 2
)

legend(
	x = -1,
	y = 102,
	legend = c("Increased statewide",
						 "Decreased statewide",
						 "No data"
					 ),
	pch = c(24,25,21),
	pt.bg = c("black",
						"white",
						"gray70"
					), 
	bty ="n"
)


axis(
	2,
	seq(-50, 100, 10),
	labels = FALSE,
	tck = -0.015
)

axis(
	2,
	seq(-50, 100, 5),
	labels = FALSE,
	tck = -0.015/2
)

mtext(
	sprintf(
		"%.0f",
		seq(-50, 100, 10)
	),
	side = 2,
	line = 0.75,
	at = seq(-50, 100, 10),
	las = 1
)

mtext(
	paste0(
		"Change in proportion of days observed ",
		"relative to historical baseline"
		),
	side = 2,
	line = 2.95,
	cex = 1.15,
	at = mean(c(-50,100))
)
}
dev.off()
####

