
############################################
#
# Proportional similarity analysis
#
# Written by M. Fidino
#
############################################

library(dplyr)
library(vegan)

# We do the same data preparation for both analyses,
#  so I moved that into it's seperate script
#  which also lets you know what object you
#  should be working with for an analysis
#  look to prepare_data.R to see how we
#  summarized it. We do slightly different 
#  summaries between the analyses (some species
#  had to be ommitted for the glm analysis).
analysis <- "alpha_beta"
source("prepare_data.R")

prop_seen <- ds %>% 
	dplyr::group_by(observer, species) %>%
	dplyr::summarise(detected = sum(daySeen) / sum(countOfDays) )

alphas <- ds %>%
	dplyr::group_by(observer, species) %>%
	dplyr::summarise(detected = sum(daySeen) > 0 ) %>% 
	dplyr::ungroup() %>% 
	dplyr::group_by(observer) %>% 
	dplyr::summarise(a = sum(detected))

# Split into a list
sp_dat <- split(
	prop_seen,
	prop_seen$observer
)

# Dreuth vs Walter
1 - vegan::vegdist(
	rbind(
		sp_dat$Dreuth$detected,
		sp_dat$Walter$detected
	),
	method = "jaccard"
)
# Fidino vs Walter
1 - vegan::vegdist(
	rbind(
		sp_dat$Fidino$detected,
		sp_dat$Walter$detected
	),
method = "jaccard"
)
# Fidino vs Dreuth
1 - vegan::vegdist(
	rbind(
		sp_dat$Fidino$detected,
		sp_dat$Dreuth$detected
	),
	method = "jaccard"
)
