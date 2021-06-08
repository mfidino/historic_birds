library(vegan)

analysis <- "richness"
source("prepare_data.R")

# determine the number of species
gen_comm_mat <- function(observer, data){
	# reduce to walters
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
											nrow = dsamp$countOfDays[yr]
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
				tmp_mat[1:tmpdat$daySeen, my_col] <- 1
			}
		}
		cm_list[[yr]] <- tmp_mat
	}
	
	com_mat<- do.call(rbind, cm_list)
	
	return(com_mat)
	
}

walter <- gen_comm_mat("Walter", ds)

dreuth <- gen_comm_mat("Dreuth", ds)

fidino <- gen_comm_mat("Fidino", ds)


w_sac <- specaccum(walter)
d_sac <- specaccum(dreuth)
f_sac <- specaccum(fidino)

w_ri <- specpool(walter)
d_ri <- specpool(dreuth)
f_ri <- specpool(fidino)

floor(qnorm(c(0.025, 0.975), d_ri$chao, d_ri$chao.se))

to_plot <- data.frame(est = c(
	w_ri$chao,
	d_ri$chao,
	f_ri$chao
))

to_plot$low <- to_plot$est - (1.96 * c(w_ri$chao.se,
															 d_ri$chao.se,
															 f_ri$chao.se))

to_plot$hi <- to_plot$est + ( 1.96 * c(w_ri$chao.se,
															d_ri$chao.se,
															f_ri$chao.se))

to_plot$obs <- factor(c("Walter", "Dreuth", "Fidino"),
											levels = c("Walter", "Dreuth", "Fidino"))

pdf(
	"./figures/lp_specaccum.pdf",
	height = 6,
	width = 6,
)

plot(w_sac, ylim = c(0, 130), ci.type = "polygon",
		 col = "black", ci.col = "gray", lwd = 2,
		 xlab = "Number of bird counts",
		 ylab = "Species richness", bty = 'l',
		 cex.lab = 1.5)


plot(d_sac, add = TRUE, col =  "black", ci.type = "polygon",
		 ci.col = scales::alpha("purple", .20), lty = 2, lwd = 2)

plot(f_sac, add = TRUE, col =  "black", ci.type = "polygon",
		 ci.col = scales::alpha("yellow", .20), lty = 3, lwd = 2)

legend("bottomright" , legend = c("Walter  (1898 - 1903)", "Dreuth (1927 - 1932)", "Fidino  (2012 - 2015)"),
			 lty = 1:3, lwd = 2, bty="n", cex = 1.5)

dev.off()

