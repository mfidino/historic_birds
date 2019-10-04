##########################
#
# Historical bird analysis
#  of days species seen 
#  per year
#
# Written by M. Fidino
#
##########################

# Necessary packages for this analysis
library(dplyr)
library(car)
library(emmeans)
library(vegan)

# We do the same data preparation for both analyses,
#  so I moved that into it's seperate script
#  which also lets you know what object you
#  should be working with for an analysis
#  look to prepare_data.R to see how we
#  summarized it. We do slightly different 
#  summaries between the analyses (some species
#  had to be ommitted for the glm analysis).
analysis <- "binomial"
source("prepare_data.R")

# This is just a summary from the raw data.

# Grouping by all categories we calculate the proportion of days
#  each species was observed per year
#  Then we calculate the mean proportion and sd per observer for each species
sp_summary <- ds %>% 
	dplyr::group_by(species, observer, year) %>% 
	dplyr::summarise(prop_seen = daySeen / countOfDays) %>% 
	dplyr::ungroup() %>%
	dplyr::group_by(species, observer) %>% 
	dplyr::summarise(mu = mean(prop_seen), sd = sd(prop_seen))

################################
# The analysis
################################

# Split the ds data.frame into a list so we can
#  analyze each species seperately
ds <- split(
	ds,
	factor(
		ds$species
	)
)

# The number of species to analyze
n_species <- length(
	ds
)

# A list to hold model output and model coefficients
mod_summary <- coef_list<- vector(
	"list",
	length = n_species
)

# These are vectors to indicate what group a species is in
has_difference <- rare <- relation <-  rep(
	NA, 
	length = n_species
)

# For loop to analyze each species
for(i in 1:n_species){
	
	# A temporary dataframe so we don't have to index ds that whole time
	tmp <- ds[[i]]
	# drop observer period if they did not observe the species at all
	my_sum <- tmp %>% 
		dplyr::group_by(observer) %>% 
		dplyr::summarise(count = sum(daySeen))
	
	if(any(my_sum$count == 0)) {
		# The observer to remove
		obs <- my_sum$observer[my_sum$count>0]
		# updated tmp without observer
		tmp <- tmp[which(tmp$observer %in% obs),]
	}
  # This is the response matrix for our binomial glm
	#  2 columns (respectively number of successes and failures)
	tmp_y <- cbind(
		tmp$daySeen,
		tmp$countOfDays - tmp$daySeen
	)
	# the binomial glm
	tmp_mod <- glm(
		tmp_y ~ observer,
		data = tmp,
		family = binomial
	)
	# Determine if there is a significant difference between observers
	#  at 0.05 level (as TRUE/FALSE statement)
	has_difference[i] <- car::Anova(tmp_mod)$`Pr(>Chisq)` < 0.05
	
	# compare each group to one another
	my_contrasts <- data.frame(
		emmeans::lsmeans(
			tmp_mod, pairwise ~ observer
		)$contrasts
	)
	
	# the model coefficients
	tmp_coefs <- coefficients(
		tmp_mod
	)

	# Get model predictions on probability scale
	to_pred <- data.frame(
		observer = 
			factor(
				unique(tmp$observer),
				levels = c("Walter","Dreuth", "Fidino")
		  )
	)

	# Predict on logit scale first to get standard errors
	fits <- predict(
		tmp_mod,
		newdata = to_pred,
		type='link',
		se.fit = TRUE
	)	
  
	# Calculate 95% confidence intervals
	lower <- plogis(
		qnorm(
			0.025,
			fits$fit,
			fits$se.fit
		)
	)
	upper <- plogis(
		qnorm(
			0.975,
			fits$fit,
			fits$se.fit
		)
	)
	
	# This is the model output we want
	to_return <- data.frame(
		species = names(ds)[i],
		est = plogis(fits$fit),
		lower,
		upper, 
		observer = to_pred$observer,
		rank = rank(plogis(fits$fit))
	)
	# From here we need to determine which category a species is in
	#  e.g., did they increase, decrease, etc.
	#  We are going to rank from 1, 2, and 3, where 1 is the lowest.
	
	# do summary for when there is only 2 time observations
	
	# NO WALTER DATA
		if(!'Walter' %in% to_pred$observer){
			# put a 0 for Walter
			tmp_rank <- rank(
				c(
					0, to_return$est
				)
			)
			# if significant use tmp_rank
			#  other wise the two other survey periods
			#  are equivalent (statistically speaking)
			if(my_contrasts$p.value < 0.05){
			relation[i] <- paste(
				tmp_rank,
				collapse = "-"
			)
			} else {
				relation[i] <- "1-2-2"
			}
			next
		}
	# NO DREUTH DATA
		if(!'Dreuth' %in% to_pred$observer){
			# Add 0 for Dreuth
			tmp_rank <- rank(
				c(
					to_return$est[1],
					0,
					to_return$est[2]
				)
			)
			# if significant use tmp_rank
			#  other wise the two other survey periods
			#  are equivalent (statistically speaking)
			if(my_contrasts$p.value < 0.05){
				relation[i] <- paste(
					tmp_rank,
					collapse = "-"
				)
			} else {
				relation[i] <- "2-1-2"
			}
			next	
		}
	  # NO FIDINO DATA
		if(!'Fidino' %in% to_pred$observer){
			# Add 0 for Fidino
			tmp_rank <- rank(
				c(
					to_return$est,
					0
				)
			)
			# if significant use tmp_rank
			#  other wise the two other survey periods
			#  are equivalent (statistically speaking)
			if(my_contrasts$p.value < 0.05){
				relation[i] <- paste(
					tmp_rank,
					collapse = "-"
				)
			} else {
				relation[i] <- "2-2-1"
			}
			next
		}
	
	# Moving onto when we have data for all 3 survey periods
	
	# Were there no differences across time? 
	#  If so, was the species rare (detected on less than 5 percent of days)
	if(!has_difference[i]){
		relation[i] <- "1-1-1"
		rare[i] <- plogis(tmp_coefs[1]) < 0.05
		next
	}
	
	# If all survey periods are different, rank them
	if(all(my_contrasts$p.value < 0.05)){
		relation[i] <- paste(
			to_return$rank,
			collapse = "-"
		)
	}
	
	# If Walter vs Dreuth is not significant the others are
	if(my_contrasts$p.value[1] > 0.05 & all(my_contrasts$p.value[-1] < 0.05)){
		# Give Walters the same estimate as Dreuth
		to_return$est[1:2] <- to_return$est[1] 
		# Rank them
		to_return$rank <- rank(
			to_return$est,
			ties.method = 'min'
		)
	  relation[i] <- paste(
	  	to_return$rank,
	  	collapse = "-"
	  )
	}
	# Walter vs Dreuth is the only significant difference
	if(my_contrasts$p.value[1] < 0.05 & all(my_contrasts$p.value[-1] > 0.05)){
		# Was Walter lower or higher than Dreuth?
		if(my_contrasts$estimate[1] < 0){
			relation[i] <- '1-2-2'
		}
		if(my_contrasts$estimate[1] > 0){
			relation[i] <- '2-1-1'
		}
	}
	# Walter vs Fidino is the only significant difference
	if(my_contrasts$p.value[2] < 0.05 & all(my_contrasts$p.value[-2] > 0.05)){
		# Was Fidino lower or higher than Walter?
		if(my_contrasts$estimate[2] < 0){
			relation[i] <- '1-1-2'
		}
		if(my_contrasts$estimate[2] > 0){
			relation[i] <- '2-2-1'
		}
	}
	# Dreuth vs Fidino is the only significant difference
	if(my_contrasts$p.value[3] < 0.05 & all(my_contrasts$p.value[-3] > 0.05)){
		# Was Fidino lower or higher than Dreuth?
		if(my_contrasts$estimate[3] < 0){
			relation[i] <- '1-1-2'
		}
		if(my_contrasts$estimate[3] > 0){
			relation[i] <- '2-2-1'
		}
	}
	# Walter vs Fidino not significant, everything else is
	#  Will determine if Dreuth is highest or lowest
	if(my_contrasts$p.value[2] > 0.05 & all(my_contrasts$p.value[-2] < 0.05) ){
		to_return$est[c(1,3)] <- to_return$est[1] 
		to_return$rank <- rank(
			to_return$est,
			ties.method = 'min'
		)
		relation[i] <- paste(
			to_return$rank,
			collapse = "-"
		)
	}
	# Dreuth vs Fidino not significant, everything else is
	#  Will determine if Walter is highest or lowest
	if(my_contrasts$p.value[3] > 0.05 & all(my_contrasts$p.value[-3] < 0.05) ){
		if(to_return$rank[1] == 1){
			to_return$rank <- c(1,2,2)
		}
		if(to_return$rank[1] == 3){
			to_return$rank <- c(2,1,1)
		}
		relation[i] <- paste(
			to_return$rank,
			collapse = "-"
		)
	}
	# Check if all contrasts are not significant
	#  Different from above because we are correcting for
	#  multiple testing here
	if(all(my_contrasts$p.value > 0.05)){
		relation[i] <- '1-1-1'
		rare[i] <- plogis(tmp_coefs[1]) < 0.05
	}
	
	# Put the model in mod_summary
	mod_summary[[i]] <- tmp_mod
	# Put the predictions in coef_list
	coef_list[[i]] <- to_return
}

