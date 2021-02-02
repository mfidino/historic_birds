library(vegan)

analysis <- "richness"
source("sourcer.R")
source("prepare_data.R")

# determine the number of species
gen_beta_mat <- function(observer, data){
	# reduce to observer
	dsub <- data[which(data$observer == observer),]
	
	# get richness
	nspec <- dplyr::n_distinct(dsub$species)
	
	# figure out number of samps per year
	dsamp <- dsub %>% 
		dplyr::select(year, countOfDays) %>% 
		dplyr::distinct()
	
	
	# make a community matrix for each year, which
	#  we will bind together
	
	all_species <- sort(unique(data$species))
	sp_insamp <- sort(unique(ds$species))
	cm_list <- vector("list", length = nrow(dsamp))
	
	for(yr in 1:nrow(dsamp)){
		tmp_mat <- matrix(0,
											ncol = nspec,
											nrow = 1
		)
		for(sp in 1:length(sp_insamp)){
			my_col <- which(all_species == sp_insamp[sp])
			tmpdat <- dsub[
				dsub$species == sp_insamp[sp] &
					dsub$year == dsamp$year[yr],
				]
			if(tmpdat$daySeen == 0){
				next
			} else {
				tmp_mat[1, my_col] <- tmpdat$daySeen / tmpdat$countOfDays
			}
		}
		cm_list[[yr]] <- tmp_mat
	}
	
	com_mat<- do.call(rbind, cm_list)
	
	return(com_mat)
	
}

walter <- gen_beta_mat("Walter", ds)
dreuth <- gen_beta_mat("Dreuth", ds)
fidino <- gen_beta_mat("Fidino", ds)

all_mats <- rbind(walter, dreuth, fidino)

# Apply a permutational ANOVA

tp <- ds[,c("year", "observer")] 
tp <- tp[!duplicated(tp),]

lp_perm <- adonis(
	all_mats ~ tp$observer
)

# plot out the community change

lp_nmds <- metaMDS(
	all_mats,
	try = 100
)

pdf(
	"beta_nmds.pdf",
	height = 4,
	width = 4,
)

plot(
	lp_nmds,
	type = "n",
	bty = "l"
)
points(
	lp_nmds,
	display = "sites",
	pch = c(16, 8, 17) [as.numeric(tp$observer)],
	col = c("blue", "orange", "black") [as.numeric(tp$observer)]
)
legend(
	"topright",
	legend = c(
		"1898 - 1903",
		"1927 - 1932",
		"2012 - 2015"
	),
	pch = c(16, 8, 17),
	col = c("blue", "orange", "black"),
	bty = "n",
	cex = 0.75
) 
legend(
	"bottomright",
	"Stress = 0.07",
	bty = "n",
	cex = 0.75
)  

dev.off()
