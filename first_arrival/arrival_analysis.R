###################################################
#
# Analysis of arrival date of birds at Lincoln Park
# Written by Mason Fidino
#
###################################################

# packages needed
library(tidyverse)
library(lme4)
library(msm)

# read in the data
dat <- read.csv("../data/arrival.csv", stringsAsFactors = FALSE)

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
dat <- dat[-which(dat$species %in% sp_drop),]

to_go <- c("Nonbreeding", "Year-round")

sp_drop <- sp_hist$species[which(sp_hist$status %in% to_go)]
dat <- dat[-which(dat$species %in% sp_drop),]
sp_hist <- sp_hist[-which(sp_hist$status == to_go),]

# we need to drop some of the less common species. Figure out which
#  of the observers saw a given species.
sp_count <- table(dat$species, dat$observer)

# Next step. Grab the species who were seen all three time periods.
all_three <- names(which(rowSums(sp_count>0) == 3))

dat <- dat[which(dat$species %in% all_three),]
sp_hist <- sp_hist[which(sp_hist$species %in% dat$species),]

# next step. Grab the species who have been seen atleast 9 times.
gte9 <- names(which(table(dat$species) > 8))

dat <- dat[which(dat$species %in% gte9),]
sp_hist <- sp_hist[which(sp_hist$species %in% dat$species),]


#sp_hist$status[sp_hist$status == "Nonbreeding"] <- "Migrating"
# make year start at 0 for the minimum.
dat$year0 <- dat$year - min(dat$year)
# do it by decade instead
dat$year0 <-dat$year0 / 10

dat <- left_join( sp_hist, dat, by = "species" )

# drop out of range birds
#dat <- dat[-which(dat$status == "Out of range"),]
#sp_hist <- sp_hist[-which(sp_hist$status == "Out of range"),]

dat <- dat[complete.cases(dat),]
dat$species <- factor(dat$species)
dat$status <- factor(dat$status, levels = c("Breeding", "Migrating"))
dat$family <- factor(dat$family)

the_sp <- sapply(the_dat, function(x) unname(unique(as.character(x$species))))


results_list <- vector("list", length = 61)
coef_list <- vector("list", length = 61)

the_dat <- split(dat, factor(dat$species))

for(i in 1:61){
	results_list[[i]] <- summary(lm(julian ~ year0, data = the_dat[[i]]))
	coef_list[[i]] <- data.frame(coefficients(results_list[[i]]))
}

coef_df <- bind_rows(coef_list)

coef_df$species <- rep(the_sp, each = 2)

inters <- coef_df[seq(1,121,2),]

windows(4,8)
par(mfrow = c(3,1))

hist(dat$julian[dat$observer == "Walter"])
hist(dat$julian[dat$observer == "Dreuth"])
hist(dat$julian[dat$observer == "Fidino"])

# fit random effect model
m1 <- lmer(julian ~ year0  +  (1 + year0 | species), data = dat,
					 control=lmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=2e6) ))






# get the random effects
ranvar <- ranef(m1, condVar = TRUE)

# this is the variance covariance matrix for 
#  each family
ranvar <- attr(ranvar[[1]], "postVar")

# this just gets the variance estimates
ranvar <-  t(apply(ranvar, 3, diag))

# assuming independence between fixed effects and random effects
#  for an approximate estimate of variance.
ranvar[,1] <- ranvar[,1] + vcov(m1)[1,1] # intercept
ranvar[,2] <- ranvar[,2] + vcov(m1)[2,2] # slope

# sqrt to get standard error
ransd <- sqrt(ranvar)

# collect the model coefficients
c1 <- coefficients(m1)$species

# this is to get the quantiles
myfun <- function(x){
	qnorm(c(0.025,0.5,0.975), x[1], x[2])
}

# parameter estimate with standard errors
intercepts <- cbind(c1[,1], ransd[,1])
slopes <- cbind(c1[,2], ransd[,2])

# 95% CI for the intercept and slope
intercept_estimates <- t(apply(intercepts, 1, myfun))
slope_estimates <- t(apply(slopes, 1, myfun))

plot(intercept_estimates[,2] ~ slope_estimates[,2], bty = "l",
		 ylim = c(85, 140), xlim = c(-1.75,1.75), pch = 16,
		 xlab = "Change in arrival date per decade",
		 ylab = "Average arrival date in 1897 (Julian day)")

for(i in 1:61){
	lines(x = slope_estimates[i,-2], y = rep(intercept_estimates[i,2],2))
}

abline(v = 0, lty = 3)

# Calculate the species who had negative effects
decre <- row.names(coef(m1)$species)[which(rowSums(sign(slope_estimates)) == -3)]
incre <- row.names(coef(m1)$species)[which(rowSums(sign(slope_estimates)) == 3)]

for(i in 1:length(decre)){
	# location of which species
	the_species <- which(rowSums(sign(slope_estimates)) == -3)[i]
	# the text to put there
	text(x = slope_estimates[the_species,2],
			 y = intercept_estimates[the_species,2] + 1.6,
			 labels = decre[i], cex = 0.8)
}

for(i in 1:length(incre)){
	# the location of which species
	the_species <- which(rowSums(sign(slope_estimates)) == 3)[i]
	# the text to put there
	text(x = slope_estimates[the_species,2],
			 y = intercept_estimates[the_species,2] + 1.6,
			 labels = incre[i], cex = 0.8)
}

# adding some labels to help with interpretation
text(x = -2, y = 85,labels = "Arrive earlier", cex = 1.4)
text(x = 2, y = 85,labels = "Arrive later", cex = 1.4)

# calculate confidence intervals for individual families.
#  Using lme as the output makes it easier to come up with predicions.
m1 <- lme(julian ~ year0 + status, data = dat, random = ~ 1 + year0 | species)

# newdata for prdictions
newdat <- expand.grid(year0=seq(-5,12.5,1), species=levels(dat$species))
newdat <- left_join(newdat, sp_hist[,c("species", "status")], by = "species")
newdat$species <- factor(newdat$species)
newdat$status <- factor(newdat$status, levels = c( "Breeding", "Migrating"))

# add these predictions
newdat$pred <- predict(m1, newdat)

# [-2] drops response from formula
Designmat <- model.matrix(formula(m1)[-2], newdat)

# The prediction
predvar <- diag(Designmat %*% vcov(m1) %*% t(Designmat)) 

# standard error
newdat$SE <- sqrt(predvar)

# for predictive interval
newdat$SE2 <- sqrt(predvar+m1$sigma^2)

# assuming normal distribution for intervals
# 95% confidence interval
newdat$low <- qnorm(0.025, newdat$pred, newdat$SE)
newdat$hi <- qnorm(0.975, newdat$pred, newdat$SE)
# 95% predictive interval
newdat$predlow <- qnorm(0.025, newdat$pred, newdat$SE2)
newdat$predhi <- qnorm(0.975, newdat$pred, newdat$SE2)

# which family to be plotted
pdf("species_arrival_results_decrease.pdf", height = 4, width = 4)
for(i in 1:length(decre)){

the_fam <- decre[i]
one_fam <- newdat[newdat$species == the_fam,]

# this is the raw data
family_data <- dat[dat$species == the_fam,]

# calling plot function
plot(1~1, 
		 ylim = c(30,170),xlim = c(0, 12), xlab = "", 
		 ylab = "", bty = "l", yaxt = "n", xaxt = "n")

# adding ticks to the x-axis
axis(1, at= seq(0,12, 2), labels = F, tck = -0.025)
axis(1, at= seq(0,12, 1), labels = F, tck = -0.0125)
# adding numbers to the x-axis
mtext(text = seq(0,12,2), 
			1, line = 0.45, at = seq(0,12,2))
# label to the x-axis
mtext(text = "Decades since 1897", 1, line = 2.7, at = 6,
			cex = 1)

# following suit to the y-axis
axis(2, at= seq(30,170, 20), labels = F, tck = -0.025)
axis(2, at= seq(30,170, 10), labels = F, tck = -0.0125)
mtext(text = seq(30,170,20), 
			2, line = 0.6, at = seq(30,170,20), las = 2)
mtext(text = "Arrival date (Julian day)", 2, line = 2.7, at = 100,
			cex = 1)

# 95% predictive interval plotting,
#  doing it as a polygon
x1 <- one_fam$year0
x2 <- rev(one_fam$year0)
y1 <- one_fam$predlow
y2 <- rev(one_fam$predhi)
polygon(c(x1, x2), c(y1, y2), col = alpha("#20B4D3", .30), border = NA)

# plotting the points
points(family_data$julian ~ family_data$year0, pch = 21, cex = 1.2, bg = "#20B4D3")
lines(one_fam$low ~ one_fam$year0, lty = 2, lwd = 2)
lines(one_fam$hi ~ one_fam$year0, lty = 2, lwd = 2)
lines(one_fam$pred ~ one_fam$year0, lwd = 3)
mtext(text = decre[i], 3, at = 6)

}
dev.off()

