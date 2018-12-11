.package_load<-function(packages = NULL, quiet=TRUE, 
											 verbose=FALSE, warn.conflicts=FALSE){
	
	# download required packages if they're not already
	
	pkgsToDownload<- packages[!(packages  %in% installed.packages()[,"Package"])]
	if(length(pkgsToDownload)>0)
		install.packages(pkgsToDownload, repos="http://cran.us.r-project.org", 
										 quiet=quiet, verbose=verbose)
	
	# then load them
	for(i in 1:length(packages))
		require(packages[i], character.only=T, quietly=quiet, 
						warn.conflicts=warn.conflicts)
}

clean_names <- function(names = NULL){
	
	# remove all dashes and replace with space
	names <- names %>% 
		gsub("-", " ", .) %>% 
		gsub("'", "", .) %>% 
		tolower(.)
	
return(names)
	
}
