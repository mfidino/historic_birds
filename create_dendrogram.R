#########################################
#
# create the dendrogram for figure 2
#
#  Written by M. Fidino
#
#########################################

# This script just constructs the hierarchy we need
#  I moved the plot it created to inkscape to 
#  spruce it up.

library(data.tree)

# Conduct the analysis
source("days_seen_analysis.R")

# A utility function to  get the specie names we need
my_spe <- function(x, sp_dat = sp_hist, us = unq_species){
	y <- sp_hist[which(sp_hist$species %in% us[x]), c('species', 'family')]
	return(y[order(y$family, y$species),])
}

# We need to construct our hierachy, we use different 'group'
#  columns to do this
common_spec <- sp_hist[which(sp_hist$species %in% unq_species[which(!rare)]),]
common_spec <- common_spec[order(common_spec$family, common_spec$species), c("species", "family")]
common_spec$group1 <- "No change"
common_spec$group2 <- "Common"
common_spec$group3 <- NA
common_spec$group4 <- NA

rare_spec <- sp_hist[which(sp_hist$species %in% unq_species[which(rare)]),]
rare_spec <- rare_spec[order(rare_spec$family, rare_spec$species), c("species", "family")]
rare_spec$group1 <- "No change"
rare_spec$group2 <- "Rare"
rare_spec$group3 <- NA
rare_spec$group4 <- NA

# ELBOWED
mid_worst1 <- which(relation %in% c('2-1-2'))
mid_worst1 <- my_spe(mid_worst1)
mid_worst1$group1 <- "Change"
mid_worst1$group2 <- "Elbowed"
mid_worst1$group3 <- "Mid-worst"
mid_worst1$group4 <- "2-1-2"

mid_worst2 <- which(relation %in% c('3-1-2'))
mid_worst2 <- my_spe(mid_worst2)
mid_worst2$group1 <- "Change"
mid_worst2$group2 <- "Elbowed"
mid_worst2$group3 <- "Mid-worst"
mid_worst2$group4 <- "3-1-2"

mid_worst3 <- which(relation %in% c('2-1-3'))
mid_worst3 <- my_spe(mid_worst3)
mid_worst3$group1 <- "Change"
mid_worst3$group2 <- "Elbowed"
mid_worst3$group3 <- "Mid-worst"
mid_worst3$group4 <- "2-1-3"

mid_best1 <- which(relation %in% c('1-2-1', '1-3-1'))
mid_best1 <- my_spe(mid_best1)
mid_best1$group1 <- "Change"
mid_best1$group2 <- "Elbowed"
mid_best1$group3 <- "Mid-best"
mid_best1$group4 <- "1-2-1"

mid_best2 <- which(relation %in% c('2-3-1'))
mid_best2 <- my_spe(mid_best2)
mid_best2$group1 <- "Change"
mid_best2$group2 <- "Elbowed"
mid_best2$group3 <- "Mid-best"
mid_best2$group4 <- "2-3-1"

mid_best3 <- which(relation %in% c('1-3-2'))
mid_best3 <- my_spe(mid_best3)
mid_best3$group1 <- "Change"
mid_best3$group2 <- "Elbowed"
mid_best3$group3 <- "Mid-best"
mid_best3$group4 <- "1-3-2"

# Monotonic down
mn1 <- which(relation == '3-2-1')
mn1 <- my_spe(mn1)
mn1$group1 <- "Change"
mn1$group2 <- "Monotonic"
mn1$group3 <- "Down"
mn1$group4 <- "3-2-1"

mn2 <- my_spe(which(relation == '2-2-1'))
mn2$group1 <- "Change"
mn2$group2 <- "Monotonic"
mn2$group3 <- "Down"
mn2$group4 <- "2-2-1"

# Montonic up
mn3 <- my_spe(which(relation == '1-2-3'))
mn3$group1 <- "Change"
mn3$group2 <- "Monotonic"
mn3$group3 <- "Up"
mn3$group4 <- "1-2-3"

mn4 <- my_spe(which(relation %in% c('1-1-2', '1-1-3')))
mn4$group1 <- "Change"
mn4$group2 <- "Monotonic"
mn4$group3 <- "Up"
mn4$group4 <- "1-1-2"

mn5 <- my_spe(which(relation == '2-1-1'))
mn5$group1 <- "Change"
mn5$group2 <- "Monotonic"
mn5$group3 <- "Down"
mn5$group4 <- "2-1-1"

mn6 <- my_spe(which(relation == '1-2-2'))
mn6$group1 <- "Change"
mn6$group2 <- "Monotonic"
mn6$group3 <- "Up"
mn6$group4 <- "1-2-2"


# Bind all of this together
complete_tree<- rbind.data.frame(
	common_spec, rare_spec, mid_worst1,
	mid_worst2, mid_worst3, mid_best1, 
	mid_best2, mid_best3, mn1, mn2, mn5, 
	mn3, mn4, mn6
)

# data.tree needs all of the group info as a string
#  seperated by a slash
complete_tree$pathString <- paste(
	"Change or No change",
	complete_tree$group1,
	complete_tree$group2,
	complete_tree$group3,
	complete_tree$group4,
	complete_tree$species,
	sep = "/"
)
# this is all of the no change species
no_change <- which(complete_tree$group1 == "No change")

complete_tree$pathString[no_change] <- paste(
	"Change or no change",
	complete_tree$group1[no_change],
	complete_tree$group2[no_change],
	complete_tree$species[no_change],
	sep = "/"
)
my_tree <- data.tree::as.Node(complete_tree)

# Create a really big dendrogram as an svg to we
#  can open it up as a vector file.
svg("./days_seen/my_complete_tree_big_dendro.svg", 20, 20 )
plot(
	as.dendrogram(
		my_tree
	),
	center = TRUE,
	cex = 0.8
)
dev.off()


