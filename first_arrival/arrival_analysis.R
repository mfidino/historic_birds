library(tidyverse)
library(lme4)
library(mvtnorm)

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
sp_hist <- sp_hist[which(sp_hist$species %in% dat$species),]

# make year start at 0 for the minimum.
dat$year0 <- dat$year - min(dat$year)

dat <- left_join( sp_hist, dat, by = "species" )

# drop out of range birds
dat <- dat[-which(dat$status == "Out of range"),]

dat <- dat[complete.cases(dat),]
dat$status <- factor(dat$status, levels = c("Year-round", "Breeding", "Migrating", "Nonbreeding"))
dat$family <- factor(dat$family)

# fit random effect model

m1 <- lmer(julian ~ year0 +  (1 + year0 | family), data = dat)


ranvar <- ranef(m1, condVar = TRUE)

# this is the variance covariance matrix for 
#  each family
ranvar <- attr(ranvar[[1]], "postVar")

ranvar <-  t(apply(ranvar, 3, diag))

# assuming independence between fixed effects and random effects
#  for an approximate estimate of variance.
ranvar[,1] <- ranvar[,1] + vcov(m1)[1,1]
ranvar[,2] <- ranvar[,2] + vcov(m1)[2,2]

# sqrt to get standard error
ransd <- sqrt(ranvar)

# collect the model coefficients
c1 <- coefficients(m1)$family

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
		 ylim = c(75, 135), xlim = c(-0.45,0.45), pch = 16,
		 xlab = "Change in arrival date per year",
		 ylab = "Average arrival date in 1897 (Julian day)")

for(i in 1:26){
	lines(x = slope_estimates[i,-2], y = rep(intercept_estimates[i,2],2))
}

abline(v = 0, lty = 3)


# Calculate the species who had negative effects
decre <- levels(dat$family)[which(rowSums(sign(slope_estimates)) == -3)]
incre <- levels(dat$family)[which(rowSums(sign(slope_estimates)) == 3)]


which(rowSums(sign(slope_estimates)) == -3)

for(i in 1:length(decre)){
	the_species <- which(rowSums(sign(slope_estimates)) == -3)[i]
	
	text(x = slope_estimates[the_species,2],
			 y = intercept_estimates[the_species,2] + 1.6,
			 labels = decre[i], cex = 0.8)
	
}

for(i in 1:length(incre)){
	the_species <- which(rowSums(sign(slope_estimates)) == 3)[i]
	
	text(x = slope_estimates[the_species,2],
			 y = intercept_estimates[the_species,2] + 1.6,
			 labels = incre[i], cex = 0.8)
	
}

text(x = -0.2, y = 85,labels = "Arrive earlier", cex = 1.4)
text(x = 0.2, y = 85,labels = "Arrive later", cex = 1.4)






# calculate confidence intervals for individual families.

m1 <- lme(julian ~ year0, data = dat, random = ~ 1 + year0 | family)


newdat <- expand.grid(year0=seq(0,118,1), family=levels(dat$family))
newdat$pred <- predict(m1, newdat)



## [-2] drops response from formula
Designmat <- model.matrix(formula(m1)[-2], newdat)
predvar <- diag(Designmat %*% vcov(m1) %*% t(Designmat)) 
newdat$SE <- sqrt(predvar) 
newdat$SE2 <- sqrt(predvar+m1$sigma^2)

newdat$low <- qnorm(0.025, newdat$pred, newdat$SE)
newdat$hi <- qnorm(0.975, newdat$pred, newdat$SE)
newdat$predlow <- qnorm(0.025, newdat$pred, newdat$SE2)
newdat$predhi <- qnorm(0.975, newdat$pred, newdat$SE2)

the_fam <- "Fringillidae"
one_fam <- newdat[newdat$family == the_fam,]

hm <- dat[dat$family == the_fam,]

plot(hm$julian ~ hm$year0, ylim = c(50,150), xlab = "Years since 1897", 
		 ylab = "Arrival date (Julian day)", bty = "l")

x1 <- one_fam$year0
x2 <- rev(one_fam$year0)
y1 <- one_fam$predlow
y2 <- rev(one_fam$predhi)
polygon(c(x1, x2), c(y1, y2), col = alpha("#20B4D3", .30), border = NA)


points(hm$julian ~ hm$year0, pch = 21, cex = 1.2, bg = "#20B4D3")
lines(one_fam$low ~ one_fam$year0, lty = 2, lwd = 2)
lines(one_fam$hi ~ one_fam$year0, lty = 2, lwd = 2)

lines(one_fam$pred ~ one_fam$year0, lwd = 3)



lines(one_fam$predlow ~ one_fam$year0, lty = 3)
lines(one_fam$predhi ~ one_fam$year0, lty = 3)

plot(c1$`(Intercept)` ~ c1$year0, ylab = "average arrival date in 1897 (Julian day)",
		 xlab = "Change in arrival date per year", bty = "l")

t1 <- dat[which(dat$family == "Paridae"),]
t2 <- dat[which(dat$family == "Fringillidae"),]



plot(t1$julian ~ t1$year0)
plot(t2$julian ~ t2$year0, bty="l", xlab = "Years since 18970", ylab = "Arrival date")
test <- dat %>% 
	group_by(species) %>% 
	nest() %>% 
	mutate(model = map(data, ~lm(julian ~ year0, data = .)))
	

