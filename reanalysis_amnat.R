
fun <- read.csv("./data/BirdFuncDat.csv",
								stringsAsFactors = FALSE)
fun$English <- gsub("Grey", "Gray", fun$English)
fun$English <- gsub("grey", "gray", fun$English)
fun$English <- gsub("Wood-pewee", "Wood-Pewee", fun$English)

ward$species[which(!ward$species %in% fun$English)]

fun <- fun[which(fun$English %in% ward$species),]

f2 <- fun[which(fun$English %in% test$species),]

hm <- apply(f2[,10:19], 1, var)

f2$fbreath <- hm

hm2 <- apply(f2[,24:30], 1, var)
f2$strat <- hm2

# join ds to ward data
fun$English[ grep("\\-Pewee", fun$English)]
fun$English[ grep("virens", fun$Scientific)]

ward <- read.csv("./data/ward_changes.csv",
								 stringsAsFactors = FALSE)
# Do some slight changes to species names
ward$species <- gsub("N\\.", "Northern", ward$species)

	ward$species[which(!ward$species %in% ds$species)]
unique(ds$species)

test <- dplyr::inner_join(ds, ward ,by = "species")
test <- dplyr::inner_join(test, f2[,c("English", "BodyMass.Value")],
													by = c("species" = "English"))

install.packages('lme4')
library(lme4)
test$species <- factor(test$species)
test$BodyMass.Value <- scale(test$BodyMass.Value)

test$num <- as.numeric(test$observer) - 2
test$mid <- as.numeric(test$observer == "Dreuth")
test$change <- test$change * 10

y <- cbind(test$daySeen, test$countOfDays - test$daySeen)



yo <- glmer(y ~ change + num + mid + BodyMass.Value + (1+num+mid|species),
						family = binomial, data = test)

co <- coef(yo)
bb <- glm(y ~ change + observer * species,
					data = test, family = binomial)


ss <- getME(yo,c("theta","fixef"))
m2 <- update(yo,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


m3 <- update(yo,start=ss,control=glmerControl(optimizer="bobyqa",
																					 optCtrl=list(maxfun=2e5)))



